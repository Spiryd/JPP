mod gf;
mod dhsetup;

fn main() {
    let a = gf::Gf::new(1234567);
    println!("characterisitc: {}", a.characteristic());

    let dh = dhsetup::DHSetup::new();
    println!("generator: {}", dh.get_generator());
}
