fn main() {
    cc::Build::new()
        .file("../c_implementation/rec.c")
        .compile("iter");
}