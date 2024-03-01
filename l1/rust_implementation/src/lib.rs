
pub fn factorial_iter(n: u64) -> u64 {
    (1..=n).product()
}

pub fn factorial_recursive(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * factorial_recursive(n - 1)
    }
}

pub fn gcd_iter(n: u64, k: u64) -> u64 {
    let mut n = n;
    let mut k = k;
    while n != k {
        if n > k {
            n = n - k;
        } else {
            k = k - n;
        }
    }
    n   
}

pub fn gcd_recursive(n: u64, k: u64) -> u64 {
    if k == 0 {
        n
    } else {
        gcd_recursive(k, n % k)
    }
}

pub fn diophantine_iter(a: i64, b: i64, c: i64) -> (i64, i64) {
    let mut x = 0;
    let mut y = 0;
    while x < c {
        if (c - a*x) % b == 0 {
            y = (c - a*x) / b;
            break;
        }
        x += 1;
    }
    (x, y)
}

pub fn diophantine_recursive(a: i64, b: i64, c: i64) -> (i64, i64) {
    if c == 0 {
        (0, 0)
    } else if a == 0 && b == 0 {
        (0, 0)
    } else if a == 0 {
        (0, c / b)
    } else if b == 0 {
        (c / a, 0)
    } else {
        let (x, y) = diophantine_recursive(b, a % b, c);
        (y, x - (a / b) * y)
    }
}


#[cfg(test)]
mod tester {
    #[test]
    fn test_factorial_iter() {
        assert_eq!(super::factorial_iter(5), 120);
    }
    #[test]
    fn test_factorial_recursive() {
        assert_eq!(super::factorial_recursive(5), 120);
    }
    #[test]
    fn test_gcd_iter() {
        assert_eq!(super::gcd_iter(12, 15), 3);
    }
    #[test]
    fn test_gcd_recursive() {
        assert_eq!(super::gcd_recursive(12, 15), 3);
    }
    #[test]
    fn test_diophantine_iter() {
        let (x, y) = super::diophantine_iter(2, 3, 7);
        assert_eq!(2*x + 3*y, 7);
    }
    #[test]
    fn test_diophantine_recursive() {
        let (x, y) = super::diophantine_recursive(2, 3, 7);
        assert_eq!(2*x + 3*y, 7);
    }
}
