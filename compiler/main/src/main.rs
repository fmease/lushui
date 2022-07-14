use std::process::ExitCode;

fn main() -> ExitCode {
    match driver::main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
