gcc tester.c -o a -ll1 -L../rust_implementation/target/release
LD_LIBRARY_PATH=../rust_implementation/target/release ./a
