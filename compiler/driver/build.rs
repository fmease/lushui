use std::process::Command;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo:rerun-if-changed=../../.git/index");
    println!("cargo:rerun-if-changed=../../version");

    let git = String::from_utf8(
        Command::new("git")
            .args(&["log", "-1", "--date=short", "--pretty=format:%h %cd"])
            .output()
            .unwrap()
            .stdout,
    )?;

    let version = std::fs::read_to_string("../../version")?;
    let version = version.trim();

    println!("cargo:rustc-env=VERSION={version} ({git})");

    Ok(())
}
