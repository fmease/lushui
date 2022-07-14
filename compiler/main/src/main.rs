use std::process::ExitCode;

fn main() -> ExitCode {
    match lushui_driver::main() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
