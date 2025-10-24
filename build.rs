pub fn main() {
    println!("cargo:rerun-if-changed=bindings");
    println!("cargo:rerun-if-changed=lib");
    ocaml_build::Dune::new("bindings").build()
}
