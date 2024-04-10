mod gf;

fn main() {
    let a = gf::Gf::new(3);
    let b = gf::Gf::new(5);
    println!("{}", a.characteristic());
    println!("{}", b.value());
}
