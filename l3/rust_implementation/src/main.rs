mod gf;
mod dhsetup;

fn main() {
    let a = gf::Gf::from(11);
    let m = gf::Gf::from(2115);

    let dh = dhsetup::DHSetup::new();
    println!("generator: {}", dh.get_generator());
    let mut user = dhsetup::User::<gf::Gf>::new(&dh);
    let pub_key = user.get_public_key();
    println!("public key: {}", pub_key);
    user.set_key(a);
    println!("message: {}", m);
    let c = user.encrypt(m);
    println!("encrypted: {}", c);
    let m = user.decrypt(c);
    println!("decrypted: {}", m);
}
