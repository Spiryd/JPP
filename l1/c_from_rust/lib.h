#include <stdint.h>

typedef struct {
    int64_t x;
    int64_t y;
} DiophantineSolution;

uint64_t factorial_iter(uint64_t n);
uint64_t factorial_recursive(uint64_t n);
uint64_t gcd_iter(uint64_t n, uint64_t k);
uint64_t gcd_recursive(uint64_t n, uint64_t k);
DiophantineSolution diophantine_iter(int64_t a, int64_t b, int64_t c);
DiophantineSolution diophantine_recursive(int64_t a, int64_t b, int64_t c);