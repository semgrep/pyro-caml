use std::path::{Path, PathBuf};

// from ocaml_build
pub struct Dune {
    root: PathBuf,
    library: PathBuf,
}

impl Dune {
    pub fn new(library: impl AsRef<Path>) -> Dune {
        Dune {
            root: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()),
            library: library.as_ref().to_path_buf(),
        }
    }

    pub fn with_root(mut self, root: impl AsRef<Path>) -> Dune {
        self.root = root.as_ref().to_path_buf();
        self
    }

    fn run(&self) {
        // TODO: this builds every dune package in this repo everytime
        // (including the ppx lib), we should make it only build the bindings
        let c = std::process::Command::new("dune")
            .current_dir(&self.root)
            .arg("build")
            // --root enforces dune to not search for a better dune project root
            // if we don't do this dune freaks out when building via opam
            .arg("--root")
            .arg(".")
            // for speed!
            .arg("--profile")
            .arg("release")
            .status()
            .unwrap();
        assert!(c.success());
    }

    pub fn build(self) {
        self.run();

        let path = self.root.join("_build").join("default").join(&self.library);

        let mut build = cc::Build::new();

        for file in std::fs::read_dir(&path).unwrap() {
            let file = file.unwrap();
            let path = file.path();
            if path.extension().map(|x| x.to_str().unwrap()) == Some("o") {
                build.object(&path);
            }
        }

        build.compile("ocaml");
    }
}

pub fn main() {
    // ensure rebuild if dependent ocaml libraries change
    println!("cargo:rerun-if-changed=bindings");
    println!("cargo:rerun-if-changed=lib");
    Dune::new("bindings").build()
}
