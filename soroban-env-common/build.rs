pub fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    crate_git_revision::init();
}
