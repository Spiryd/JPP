const ORDER: u64 = 1234567891;

fn sieve_of_eratosthenes(n: usize) -> Vec<usize> {
    let mut is_prime = vec![true; n + 1];
    is_prime[0] = false;
    is_prime[1] = false;
    let mut p = 2;
    while p * p <= n {
        if is_prime[p] {
            let mut i = p * p;
            while i <= n {
                is_prime[i] = false;
                i += p;
            }
        }
        p += 1;
    }
    is_prime
        .into_iter()
        .enumerate()
        .filter(|(_, is_prime)| *is_prime)
        .map(|(i, _)| i)
        .collect()
}

fn power_of_prime(n: usize) -> Result<(usize, usize), ()> {
    let mut n = n;
    for i in sieve_of_eratosthenes(n) {
        if n % i == 0 {
            let mut count = 0;
            while n % i == 0 {
                n /= i;
                count += 1;
            }
            if n == 1 {
                return Ok((i, count));
            } else {
                return Err(());
            }
        }
    }
    Err(())
}
/// Struct representing a number in GF(1234577)
#[derive(Clone, Copy, Debug)]
pub struct Gf {
    value: u64,
}   

#[allow(dead_code)]
impl Gf {
    pub fn new(value: u64) -> Self {
        Self { value: value % ORDER}
    }
    pub fn value(&self) -> u64 {
        self.value
    }
    pub fn characteristic(&self) -> u64 {
        power_of_prime(ORDER as usize).expect("SHOULD NOT EXIST").0 as u64
    }
    fn inv(&self) -> Self {
        let mut t = 0;
        let mut newt = 1;
        let mut r = ORDER as i128;
        let mut newr = self.value as i128;
        while newr != 0 {
            let quotient = r / newr;
            let tmp = newt;
            newt = t - quotient * newt;
            t = tmp;
            let tmp = newr;
            newr = r - quotient * newr;
            r = tmp;
        }
        if r > 1 {
            panic!("{} is not invertible", self.value);
        }
        if t < 0 {
            t += ORDER as i128;
        }
        Self::new(t as u64)
    }
}

/// Overloads `==`, `!=` operators
impl PartialEq for Gf {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
/// Overloads `<=,` `>=`, `<` i `>` operators
impl PartialOrd for Gf {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
    
}

impl std::fmt::Display for Gf {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Overloads `+` operator
impl std::ops::Add for Gf {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::new(self.value + other.value)
    }
}

/// Overloads `+=` operator
impl std::ops::AddAssign for Gf {
    fn add_assign(&mut self, other: Self) {
        self.value = (self.value + other.value) % ORDER;
    }
}

/// Overloads `-` operator
impl std::ops::Sub for Gf {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self::new(self.value + ORDER - other.value)
    }
}

/// Overloads `-=` operator
impl std::ops::SubAssign for Gf {
    fn sub_assign(&mut self, other: Self) {
        self.value = (self.value + ORDER - other.value) % ORDER;
    }
}

/// Overloads `*` operator
impl std::ops::Mul for Gf {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self::new(self.value * other.value)
    }
}

/// Overloads `*=` operator
impl std::ops::MulAssign for Gf {
    fn mul_assign(&mut self, other: Self) {
        self.value = (self.value * other.value) % ORDER;
    }
}

/// Overloads `/` operator
impl std::ops::Div for Gf {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        self * other.inv()
    }
}

/// Overloads `/=` operator
impl std::ops::DivAssign for Gf {
    fn div_assign(&mut self, other: Self) {
        *self *= other.inv();
    }
}

impl From<u64> for Gf {
    fn from(value: u64) -> Self {
        Self::new(value)
    }
}
