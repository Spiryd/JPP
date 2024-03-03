#include <stdio.h>
#include "lib.h"

int main() {
    printf("Factorial of 5: %llu\n", factorial(5));
    printf("GCD of 48 and 18: %llu\n", gcd(48, 18));
    DiophantineSolution solution = solve_diophantine(3, 6, 18);
    printf("Solution to the diophantine equation '3x + 6y = 18' : x = %lld, y = %lld\n", solution.x, solution.y);
    return 0;
}