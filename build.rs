#![feature(type_ascription)]
extern crate vergen;

use std::process::Command;
use std::fs;
use vergen::{ConstantsFlags, generate_cargo_keys};

fn main() {
    let mut files = Vec::new();
    for entry in fs::read_dir("src/migrations").unwrap() {
        let entry = entry.unwrap();
        if entry.file_name().into_string().unwrap().ends_with(".sql") {
            files.push(entry.path());
        }
    }

    println!("cargo:rerun-if-changed=src/migrations");
    for file in &files {
        println!("cargo:rerun-if-changed={}", file.display());
    }

    //println!("env vars: {:#?}", std::env::vars_os().collect():Vec<_>);
    if !std::env::var_os("NO_PGSANITY").is_some() {
        let out = Command::new("pgsanity")
            .args(&files)
            .output()
            .expect("Could not run pgsanity; Install with `sudo pip install pgsanity`, disable check by setting NO_PGSANITY env var");
        if !out.status.success() {
            eprintln!("pgsanity stderr:{}",std::str::from_utf8(&out.stderr).unwrap());
            eprintln!("pgsanity stdout:{}",std::str::from_utf8(&out.stdout).unwrap());
            panic!("pgsanity FAILED.");
        }
    }

    let mut flags = ConstantsFlags::all();
    flags.remove(ConstantsFlags::SEMVER);
    flags.insert(ConstantsFlags::SEMVER_FROM_CARGO_PKG);
    flags.insert(ConstantsFlags::REBUILD_ON_HEAD_CHANGE);

    generate_cargo_keys(flags).expect("Unable to generate the cargo keys!");
}
