fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let opt_level = std::env::var("OPT_LEVEL").unwrap_or_else(|_| "0".to_string());
    if opt_level != "0" {
        println!("cargo:rustc-cfg=opt_build");
    }
}
