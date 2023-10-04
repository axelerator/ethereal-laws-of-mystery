use std::process::Command;

fn main() {
    // note: add error checking yourself.
    let output = Command::new("git").args(&["rev-list", "--count", "HEAD"]).output().unwrap();
    let git_hash = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rustc-env=VERSION={}", git_hash);
}

