#include "wrapper.h"

uint64_t C_IFactor(uint16_t n)
{
    return IFactor(n);
}
uint64_t C_RFactor(uint16_t n)
{
    return RFactor(n);
}
uint64_t C_IGCD(uint64_t a, uint64_t b)
{
    return IGCD(a, b);
}
uint64_t C_RGCD(uint64_t a, uint64_t b)
{
    return RGCD(a, b);
}
struct int64_pair C_ILDE(uint64_t a, uint64_t b, uint64_t c)
{
    return ILDES(a, b, c);
}
struct int64_pair C_RLDE(uint64_t a, uint64_t b, uint64_t c)
{
    return RLDES(a, b, c);
}