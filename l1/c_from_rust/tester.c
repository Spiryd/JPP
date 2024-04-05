#include <stdio.h>
#include "lib.h"

int main() {
    printf("Factorial of 5: %llu\n", factorial_iter(5));
    printf("Factorial of 5: %llu\n", factorial_recursive(5));
    printf("GCD of 48 and 18: %llu\n", gcd_iter(48, 18));
    printf("GCD of 48 and 18: %llu\n", gcd_recursive(48, 18));
    DiophantineSolution solution = diophantine_iter(3, 6, 18);
    printf("Solution to the diophantine equation '3x + 6y = 18' : x = %lld, y = %lld\n", solution.x, solution.y);
    solution = diophantine_recursive(3, 6, 18);
    printf("Solution to the diophantine equation '3x + 6y = 18' : x = %lld, y = %lld\n", solution.x, solution.y);
    return 0;
}