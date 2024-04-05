#[no_mangle]
pub extern "C" fn factorial_iter(n: u64) -> u64 {
    (1..=n).product()
}

#[no_mangle]
pub extern "C" fn factorial_recursive(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * factorial_recursive(n - 1)
    }
}

#[no_mangle]
pub extern "C" fn gcd_iter(n: u64, k: u64) -> u64 {
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

#[no_mangle]
pub extern "C" fn gcd_recursive(n: u64, k: u64) -> u64 {
    if k == 0 {
        n
    } else {
        gcd_recursive(k, n % k)
    }
}

#[repr(C)]
pub struct DiophantineSolution {
    pub x: i64,
    pub y: i64,
}

#[no_mangle]
pub extern "C" fn diophantine_iter(a: i64, b: i64, c: i64) -> DiophantineSolution {
    let mut x = 0;
    let mut y = 0;
    while x < c {
        if (c - a*x) % b == 0 {
            y = (c - a*x) / b;
            break;
        }
        x += 1;
    }
    DiophantineSolution { x, y }
}

#[no_mangle]
pub extern "C" fn diophantine_recursive(a: i64, b: i64, c: i64) -> DiophantineSolution {
    if c == 0 {
        DiophantineSolution { x: 0, y: 0 }
    } else if a == 0 && b == 0 {
        DiophantineSolution { x: 0, y: 0 }
    } else if a == 0 {
        DiophantineSolution { x: 0, y: c / b }
    } else if b == 0 {
        DiophantineSolution { x: c / a, y: 0 }
    } else {
        let s = diophantine_recursive(b, a % b, c);
        let x = s.x;
        let y = s.y;
        DiophantineSolution { x: y, y: x - (a / b) * y }
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
        let s = super::diophantine_iter(2, 3, 7);
        let x = s.x;
        let y = s.y;
        assert_eq!(2*x + 3*y, 7);
    }
    #[test]
    fn test_diophantine_recursive() {
        let s = super::diophantine_iter(2, 3, 7);
        let x = s.x;
        let y = s.y;
        assert_eq!(2*x + 3*y, 7);
    }
}
