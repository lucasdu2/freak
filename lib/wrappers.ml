open Grammar
open Generator
open Printf

let rust_regex_wrapper (r : regex) : string =
  let rgx = realize_rust_regex r in
  sprintf "extern crate once_cell; extern crate regex;
use {
    once_cell::sync::Lazy,
    regex::Regex,
    std::env,
};

fn matcher(haystack: &str) -> bool {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r\"%s\").unwrap());
    RE.is_match(haystack)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let haystack = &args[1];
    if matcher(haystack) {
        println!(\"1\")
    } else {
        println!(\"0\")
    }
}" rgx
