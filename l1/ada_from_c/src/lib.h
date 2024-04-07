#ifdef __cplusplus
extern "C" {
#endif
#ifndef L1_LIB_H
#define L1_LIB_H
#include <stdint.h>

/*  
    I - iterative, R - recursive
    GCD - Greatest Common Divisor
    LDES - Linear Diophantine Equation Solver 
*/

uint64_t IFactor(uint16_t n);
uint64_t RFactor(uint16_t n);

uint64_t IGCD(uint64_t a, uint64_t b);
uint64_t RGCD(uint64_t a, uint64_t b);

struct int64_pair ILDES(uint64_t a, uint64_t b, uint64_t c);
struct int64_pair RLDES(uint64_t a, uint64_t b, uint64_t c);

struct int64_pair
{
    int64_t x;
    int64_t y;
};

#ifdef __cplusplus
}
#endif
#endif //L1_LIB_H