/// Struct representing a number in GF(1234577)
pub struct Gf1234577 {
    value: u64,
}

impl Gf1234577 {
    pub fn new(value: u64) -> Self {
        Self { value }
    }
}

impl std::fmt::Display for Gf1234577 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
