#ifdef __cplusplus
extern "C" {
#endif
#ifndef WRAPPER_H
#define WRAPPER_H

#include <stdint.h>

extern uint64_t IFactor(uint16_t n);
extern uint64_t RFactor(uint16_t n);
extern uint64_t IGCD(uint64_t a, uint64_t b);
extern uint64_t RGCD(uint64_t a, uint64_t b);
extern struct int64_pair ILDES(uint64_t a, uint64_t b, uint64_t c);
extern struct int64_pair RLDES(uint64_t a, uint64_t b, uint64_t c);

uint64_t C_IFactor(uint16_t n);
uint64_t C_RFactor(uint16_t n);
uint64_t C_IGCD(uint64_t a, uint64_t b);
uint64_t C_RGCD(uint64_t a, uint64_t b);
struct int64_pair C_ILDE(uint64_t a, uint64_t b, uint64_t c);
struct int64_pair C_RLDE(uint64_t a, uint64_t b, uint64_t c);


struct int64_pair
{
    int64_t x;
    int64_t y;
};

#ifdef __cplusplus
}
#endif
#endif