use std::fmt::Debug;

use rand::prelude::*;
use rand_pcg::Pcg64;
const CHARACTERISTIC: u64 = 1234577;

pub trait AlgebraicBody: std::ops::Mul<Output = Self> + std::ops::Div<Output = Self> + Copy + Debug + std::ops::MulAssign + From<u64> {}
impl<T: std::ops::Mul<Output = Self> + std::ops::Div<Output = Self> + Copy + Debug + std::ops::MulAssign + From<u64>> AlgebraicBody for T {}

fn check(suspect: u64) -> bool {
    let mut i = 2;
    let mut tmp = suspect;

    while i * i <= tmp {
        if tmp % i == 0 {
            if suspect.pow(((CHARACTERISTIC - 1) / i) as u32) == 1 {
                return false;
            }
            tmp /= i;
        } else {
            i += 1;
        }
    }
    if tmp > 1 && suspect.pow(((CHARACTERISTIC - 1) / tmp) as u32) == 1 {
        return false;
    }
    true
}

pub struct DHSetup<T: AlgebraicBody> {
    generator: T,
}

impl<T: AlgebraicBody> DHSetup<T> {
    pub fn new() -> Self {
        let mut rng = Pcg64::from_entropy();
        let mut generator = rng.gen_range(1..CHARACTERISTIC);
        while !check(generator) {
            generator = rng.gen_range(1..CHARACTERISTIC);
        }
        Self { generator: T::from(generator) }
    }
    pub fn get_generator(&self) -> T {
        self.generator
    }
    pub fn power(a: T, b: u64) -> T {
        let mut a = a;
        let mut b = b;
        let mut res: T = T::from(1);
        while b > 0 {
            if b % 2 == 1 {
                res = res * a;
            }
            a = a * a;
            b /= 2;
        }
        res
    }
}
pub struct User<'a, T: AlgebraicBody> {
    secret: u64,
    dhsetup: &'a DHSetup<T>,
    key: Option<T>,
}

impl<'a, T: AlgebraicBody> User<'a, T> {
    pub fn new<'b: 'a>(dhsetup: &'b DHSetup<T>) -> Self {
        let mut rng = Pcg64::from_entropy();
        let secret = rng.gen();
        println!("secret: {}", secret);
        Self { secret, dhsetup, key: None}
    }
    pub fn get_public_key(&self) -> T {
       DHSetup::power(self.dhsetup.get_generator(), self.secret)
    }
    pub fn set_key(&mut self, a: T) {
        self.key = Some(DHSetup::power(a, self.secret));
        println!("key: {:?}", self.key);
    }
    pub fn encrypt(&self, m: T) -> T {
        m * self.key.unwrap()
    }
    pub fn decrypt(&self, c: T) -> T {
        c / self.key.unwrap()
    }
}

mod tester {
    use crate::gf::Gf;

    use super::*;

    #[test]
    fn test_power() {
        let a: Gf = Gf::from(2);
        let b = 10;
        assert_eq!(DHSetup::power(a, b).value(), 1024);
    }
}
