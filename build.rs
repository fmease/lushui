use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=.git/index");

    let git_data = String::from_utf8(
        Command::new("git")
            .args(&["log", "-1", "--date=short", "--pretty=format:%h %cd"])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();

    println!("cargo:rustc-env=GIT_DATA={git_data}");
}
