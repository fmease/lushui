use std::process::{Command, ExitStatus, Stdio};

fn main() {
    #[cfg(unix)]
    if Command::new("which")
        .arg("mold")
        .stdout(Stdio::null())
        .status()
        .ok()
        .filter(ExitStatus::success)
        .is_some()
    {
        // @Task use `-B/path/to/mold` instead if `cc` is GCC <12.1.0
        println!("cargo:rustc-link-arg=-fuse-ld=mold");
    }
}
