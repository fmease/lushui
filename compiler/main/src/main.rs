use std::process::ExitCode;

fn main() -> ExitCode {
    match driver::main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
