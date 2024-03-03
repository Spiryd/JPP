#ifndef LIBRARY_H
#define LIBRARY_H

#include <stdint.h>

typedef struct {
    int64_t x;
    int64_t y;
} DiophantineSolution;

uint64_t factorial(uint64_t n);
uint64_t gcd(uint64_t a, uint64_t b);
DiophantineSolution solve_diophantine(int64_t a, int64_t b, int64_t c);

#endif