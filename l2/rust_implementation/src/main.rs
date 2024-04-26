mod gf;

fn main() {
    let a = gf::Gf::new(10);
    let b = gf::Gf::new(-1);
    println!("{}", a / b);
}
