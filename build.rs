extern crate vergen;

use vergen::{ConstantsFlags, generate_cargo_keys};

fn main() {
    let mut flags = ConstantsFlags::all();
    flags.remove(ConstantsFlags::SEMVER);
    flags.insert(ConstantsFlags::SEMVER_FROM_CARGO_PKG);
    flags.insert(ConstantsFlags::REBUILD_ON_HEAD_CHANGE);

    generate_cargo_keys(flags).expect("Unable to generate the cargo keys!");
}
