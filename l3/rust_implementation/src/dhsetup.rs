#![allow(dead_code)]
use rand::prelude::*;
use rand_pcg::Pcg64;
const CHARACTERISTIC: u64 = 1234567891;

pub trait AlgebraicBody: std::ops::Mul + Copy + std::ops::MulAssign + From<u64> {}
impl<T: std::ops::Mul + Copy + std::ops::MulAssign + From<u64>> AlgebraicBody for T {}

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
    if tmp > 1 {
        if suspect.pow(((CHARACTERISTIC - 1) / tmp) as u32) == 1 {
            return false;
        }
    }
    true
}

pub struct DHSetup {
    generator: u64,
}

impl DHSetup {
    pub fn new() -> Self {
        let mut rng = Pcg64::from_entropy();
        let mut generator = rng.gen_range(1..CHARACTERISTIC);
        while !check(generator) {
            generator = rng.gen_range(1..CHARACTERISTIC);
        }
        Self { generator }
    }
    pub fn get_generator(&self) -> u64 {
        self.generator
    }
    pub fn power<T: AlgebraicBody<Output = T>>(a: T, b: u32) -> T {
        if b == 0 {
            return T::from(1);
        }
        if b == 1 {
            return a;
        }
        let tmp = Self::power(a, b / 2);
        if b % 2 == 0 {
            tmp * tmp
        } else {
            a * tmp * tmp
        }
    }
}
pub struct User {

}

impl User {
    pub fn new() -> Self {
        Self {}
    }
    pub fn get_public_key(&self) -> u32 {
        0
    }
    pub fn set_key(&self, a: u32) {
        todo!()
    }
    pub fn encrypt(&self, m: &str) -> String {
        todo!()
    }
    pub fn decrypt(&self, c: &str) -> String {
        todo!()
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

    #[test]
    fn dhetup_test() {
        let dh = DHSetup::new();
        assert_eq!(dh.get_generator(), 2);
    }
}
