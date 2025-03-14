# freak
A randomized differential testing framework (i.e. a fuzzer) for regex engines, applying domain-knowledge of regular expression structure.

## Steps to install and run the test harness
### Set up build environment
You can attempt to use the provided Dockerfile on your machine, which runs all the required steps to set up a container in which freak will run and build. Note that freak is *only known to be supported on Linux* due to the specific Unix-like OS interface it assumes.

In general, the Dockerfile outlines the prerequisites for a working build environment. If you choose to set things up manually (or if the Dockerfile fails for some reason), these are the explicit steps that you should take.
1. Install OCaml and the opam package manager. If you're doing this manually, see [here](https://opam.ocaml.org/doc/Install.html) --- installing opam will also install the OCaml compiler.
2. Install Rust using [rustup](https://rustup.rs/). This will also install the cargo build and package manager.
3. [Install Go](https://go.dev/doc/install) and initialize it properly, i.e. add the Go binary path to the PATH environment variable.
4. Copy/clone this repository and enter the directory.
5. Install the required dependencies from opam: dune, base64. This can be done with `opam install <package-name>`.

### Build freak
When you are in the freak directory (with all dependencies installed from opam), you should be able to build the project by simply running `dune build`.

### Run freak
You should be able to run freak after building it by invoking `dune exec -- freak`. The test harness will create a "_freak_wrappers_<id>" folder for each thread it spawns, where <id> ranges from 1 to the number of threads spawned. Mismatches that are found will be saved in a corresponding "_mismatches_found_<id>" folder.
