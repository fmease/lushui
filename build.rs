fn main() {
    println!("cargo:rerun-if-changed=.git/index");

    let git_commit_hash = String::from_utf8(
        std::process::Command::new("git")
            .args(&["rev-parse", "HEAD"])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();

    println!("cargo:rustc-env=GIT_COMMIT_HASH={}", git_commit_hash);
}
