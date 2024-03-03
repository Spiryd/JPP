#include "lib.h"

uint64_t factorial(uint64_t n) {
    uint64_t result = 1;
    for (uint64_t i = 2; i <= n; i++) {
        result *= i;
    }
    return result;
}

uint64_t gcd(uint64_t a, uint64_t b) {
    while (b != 0) {
        uint64_t t = b;
        b = a % b;
        a = t;
    }
    return a;
}

DiophantineSolution solve_diophantine(int64_t a, int64_t b, int64_t c) {
    DiophantineSolution solution = {0, 0};
    // Implementacja algorytmu rozszerzonego Euklidesa
    int64_t x = 0, y = 1;
    while (x < c) {
        if ((c - a*x) % b == 0) {
            y = (c - a*x) / b;
            break;
        }
        x += 1;
    }
    solution.x = x;
    solution.y = y;
    return solution;
}