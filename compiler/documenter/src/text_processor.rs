use super::{format::declaration_url_fragment, node::Node};
use crossbeam::thread::{Scope, ScopedJoinHandle};
use diagnostics::error::Result;
use resolver::resolve_path;
use session::Session;
use span::FileName;
use std::{
    cell::RefCell,
    fs::{self, File},
    io::{BufWriter, Error, Read, Write},
    path::{Path, PathBuf},
    process::Command,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};
use syntax::parse_path;
use utility::{HashSet, default};

pub(super) enum TextProcessor<'env> {
    None,
    AsciiDoctor(Box<RefCell<Asciidoctor<'env>>>),
}

impl<'scope> TextProcessor<'scope> {
    pub(super) fn new<'a>(
        folder: &Path,
        options: &super::Options,
        session: &'a Session<'_>,
        scope: &'scope Scope<'a>,
    ) -> Result<Self, Error> {
        if options.asciidoc {
            let doctor = Asciidoctor::new(folder, session, scope)?;
            Ok(Self::AsciiDoctor(Box::new(RefCell::new(doctor))))
        } else {
            Ok(Self::None)
        }
    }

    pub(super) fn process(
        &self,
        description: String,
        url_prefix: String,
    ) -> Result<Node<'static>, Error> {
        match self {
            Self::None => Ok(description.into()),
            Self::AsciiDoctor(asciidoctor) => {
                asciidoctor.borrow_mut().process(&description, url_prefix)
            }
        }
    }

    pub(super) fn destruct(self) -> Result<(), Error> {
        match self {
            TextProcessor::None => Ok(()),
            TextProcessor::AsciiDoctor(asciidoctor) => asciidoctor.into_inner().destruct(),
        }
    }
}

// @Note this architecture of continuously creating and destroying a new Asciidoctor instance
//       is *unfathomably* slow
pub(super) struct Asciidoctor<'scope> {
    watcher: ScopedJoinHandle<'scope, ()>,
    process: Command,
    response_context: Arc<Mutex<ResponseContext>>,
    should_watch: Arc<AtomicBool>,
    input_file: File,
    output_file: File,
    temporary_files: [PathBuf; 5],
}

impl<'scope> Asciidoctor<'scope> {
    const INPUT_FILE_NAME: &'static str = "__description.adoc";
    const OUTPUT_FILE_NAME: &'static str = "__description.html";
    const EXTENSIONS_FILE_NAME: &'static str = "__AsciidoctorExtensions.rb";
    const REQUEST_LOG_FILE_NAME: &'static str = "__requests.log";
    const RESPONSE_LOG_FILE_NAME: &'static str = "__responses.log";

    /// Create an Asciidoctor text processor with the path to the documentation folder.
    pub(super) fn new<'a>(
        path: &Path,
        session: &'a Session<'_>,
        scope: &'scope Scope<'a>,
    ) -> Result<Self, Error> {
        let input_path = path.join(Self::INPUT_FILE_NAME);
        let output_path = path.join(Self::OUTPUT_FILE_NAME);
        let extensions_path = path.join(Self::EXTENSIONS_FILE_NAME);
        let request_log_path = path.join(Self::REQUEST_LOG_FILE_NAME);
        let response_log_path = path.join(Self::RESPONSE_LOG_FILE_NAME);

        let mut process = Command::new("asciidoctor");
        process.env("LUSHUI_REQUEST_LOG_PATH", &request_log_path);
        process.env("LUSHUI_RESPONSE_LOG_PATH", &response_log_path);
        process.arg(&input_path);
        process.arg("--out-file");
        process.arg(&output_path);
        process.arg("--require");
        process.arg(&extensions_path);
        process.args(["--embedded", "--safe"]);

        let input_file = File::create(&input_path)?;
        let output_file = File::options()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(&output_path)?;
        fs::write(&extensions_path, include_str!("../include/rb/AsciidoctorExtensions.rb"))?;
        File::create(&request_log_path)?;
        File::create(&response_log_path)?;

        let response_context: Arc<Mutex<ResponseContext>> = default();
        let should_watch: Arc<AtomicBool> = default();

        let watcher = scope.spawn({
            let request_log_path = request_log_path.clone();
            let response_log_path = response_log_path.clone();
            let response_context = response_context.clone();
            let should_watch = should_watch.clone();

            move |_| {
                Self::watch_requests(
                    &request_log_path,
                    &response_log_path,
                    &response_context,
                    &should_watch,
                    session,
                );
            }
        });

        Ok(Self {
            watcher,
            process,
            response_context,
            should_watch,
            input_file,
            output_file,
            temporary_files: [
                input_path,
                output_path,
                extensions_path,
                request_log_path,
                response_log_path,
            ],
        })
    }

    // @Task document the format of this file-based IPC protocol
    fn watch_requests(
        request_log_path: &Path,
        response_log_path: &Path,
        response_context: &Mutex<ResponseContext>,
        should_watch: &AtomicBool,
        session: &Session<'_>,
    ) {
        let mut handled_requests: HashSet<String> = default();
        let mut last_log_size = 0;

        // @Beacon @Task use notify::raw_waker instead!
        while should_watch.load(Ordering::Acquire) {
            std::thread::sleep(Duration::from_millis(200));

            let request_log = std::fs::read_to_string(request_log_path).unwrap();

            if request_log.len() != last_log_size {
                last_log_size = request_log.len();

                // @Task also add a cache for the message part (AsciiDoctor seemingly subsitutes the same macro several times!)
                let unhandled_requests: Vec<_> = request_log
                    .split_terminator('\x1E')
                    .map(|request| request.split_once('\x1F').unwrap())
                    .rev()
                    .filter(|&(identifier, _)| !handled_requests.contains(identifier))
                    .collect();

                if !unhandled_requests.is_empty() {
                    let mut response_file = BufWriter::new(
                        File::options().append(true).open(response_log_path).unwrap(),
                    );

                    // @Question correctness: should this be moved into the loop?
                    let response_context = response_context.lock().unwrap();

                    for (identifier, message) in unhandled_requests {
                        // @Beacon @Task don't unwrap the Result of response() @Update don't match explicitply
                        // but send `ok\x1F{result}` or `err\x1F{message}` "over the wire"
                        let response = match Request::parse(message)
                            .unwrap()
                            .response(&response_context.url_prefix, session)
                        {
                            Ok(response) => response,
                            // @Temporary
                            Err(_) => "err".into(),
                        };
                        write!(response_file, "{identifier}\x1F{response}\x1E").unwrap();
                        handled_requests.insert(identifier.to_owned());
                    }

                    response_file.flush().unwrap();
                }
            }
        }
    }

    pub(super) fn process(
        &mut self,
        description: &str,
        url_prefix: String,
    ) -> Result<Node<'static>, Error> {
        let mut input_file = BufWriter::new(&mut self.input_file);
        write!(input_file, "{description}")?;
        input_file.flush()?;

        self.response_context.lock().unwrap().url_prefix = url_prefix;
        let status = self.process.status()?;

        if !status.success() {
            return Err(Error::other("AsciiDoctor did not exit successfully"));
        }

        let mut output = String::new();
        self.output_file.read_to_string(&mut output)?;

        Ok(Node::verbatim(output))
    }

    pub(super) fn destruct(self) -> Result<(), Error> {
        self.should_watch.store(false, Ordering::Release);
        self.watcher.join().unwrap();

        for path in self.temporary_files {
            fs::remove_file(path)?;
        }
        Ok(())
    }
}

#[derive(Default)]
struct ResponseContext {
    url_prefix: String,
}

enum Request<'a> {
    DeclarationUrl(&'a str),
}

impl<'a> Request<'a> {
    pub fn parse(message: &'a str) -> Result<Self, ()> {
        let (command, payload) = message.split_once(' ').unwrap();

        Ok(match command {
            "declaration_url" => Self::DeclarationUrl(payload),
            _ => return Err(()),
        })
    }

    pub fn response(self, url_prefix: &str, session: &Session<'_>) -> Result<String> {
        Ok(match self {
            Request::DeclarationUrl(path) => {
                let file = session.map().add(
                    FileName::Anonymous,
                    Arc::new(path.to_owned()),
                    Some(session.component().index()),
                );
                let path = parse_path(file, session)?;
                let index = resolve_path(&path, session.component().root(), session)?;
                let url_suffix = declaration_url_fragment(index, session);
                format!("{url_prefix}{url_suffix}")
            }
        })
    }
}
