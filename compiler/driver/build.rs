#![feature(generic_arg_infer, let_chains)]

use std::process::Command;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo:rerun-if-changed=../../.git/index");
    println!("cargo:rerun-if-changed=../../version");

    let commit_data = Command::new("git")
        .args(["log", "-1", "--date=short", "--pretty=format:%h,%H,%cd"])
        .output()?
        .stdout;
    let commit_data = String::from_utf8(commit_data)?;

    let [short_commit_hash, commit_hash, commit_date]: [_; _] = commit_data
        .split(',')
        .collect::<Vec<_>>()
        .try_into()
        .unwrap();

    let version = std::fs::read_to_string("../../version")?;
    let version = version.trim();

    let mut features = Vec::new();
    for (key, _) in std::env::vars_os() {
        if let Some(key) = key.to_str()
        && let Some(feature) = key.strip_prefix("CARGO_FEATURE_")
        {
            features.push(feature.to_ascii_lowercase());
        }
    }
    let features = features.join(", ");

    println!("cargo:rustc-env=VERSION={version}");
    println!("cargo:rustc-env=SHORT_COMMIT_HASH={short_commit_hash}");
    println!("cargo:rustc-env=COMMIT_HASH={commit_hash}");
    println!("cargo:rustc-env=COMMIT_DATE={commit_date}");
    println!("cargo:rustc-env=FEATURES={features}");
    println!("cargo:rustc-env=PROFILE={}", std::env::var("PROFILE")?);
    println!("cargo:rustc-env=TARGET={}", std::env::var("TARGET")?);

    Ok(())
}
