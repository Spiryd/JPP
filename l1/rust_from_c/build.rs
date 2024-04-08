fn main() {
    cc::Build::new()
        .file("../c_implementation/iter.c")
        .compile("iter");
}