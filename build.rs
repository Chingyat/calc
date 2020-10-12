extern crate cbindgen;

use std::env;
use cbindgen::Config;
use cbindgen::Language::C;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    let mut config = Config::default();
    config.language = C;
    config.cpp_compat = true;
    config.include_guard = Some(From::from("CALC_H"));

    cbindgen::Builder::new()
        .with_config(config)
        .with_crate(crate_dir)
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("calc.h");
}
