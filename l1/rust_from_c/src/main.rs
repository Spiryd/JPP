#[repr(C)]
pub struct DiophantineSolution {
    pub x: i64,
    pub y: i64,
}

extern "C" {
    pub fn factorial(n: u64) -> u64;
    pub fn gcd(a: u64, b: u64) -> u64;
    pub fn solve_diophantine(a: i64, b: i64, c: i64) -> DiophantineSolution;
}

fn main() {
    println!("Factorial of 5: {}", unsafe { factorial(5) });
    println!("GCD of 12 and 15: {}", unsafe { gcd(12, 15) });
    let solution = unsafe { solve_diophantine(3, 6, 18) };
    println!("Diophantine solution: x = {}, y = {}", solution.x, solution.y);
}
