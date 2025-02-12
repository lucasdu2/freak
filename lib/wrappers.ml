open Grammar
open Generator
open Printf

let rust_regex_wrapper (r : regex) : string =
  let real = realize_rust_regex r in
  sprintf "use {
    once_cell::sync::Lazy,
    regex::Regex,
};

fn some_helper_function(haystack: &str) -> bool {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r\"%s\").unwrap());
    RE.is_match(haystack)
}

fn main() {
    assert!(some_helper_function(\"abc\"));
    assert!(!some_helper_function(\"ac\"));
}" real
