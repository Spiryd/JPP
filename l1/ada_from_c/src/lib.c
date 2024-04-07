#include "lib.h"
#include <stdlib.h>
#include <stdio.h>

uint64_t IFactor(uint16_t n)
{
    uint64_t r = 1;

    for(uint16_t i = 2; i <= n; i++)
    {
        r *= i;
    }

    return r;
}

uint64_t RFactor(uint16_t n)
{
    if(n < 2)
    {
        return 1;
    }
    else
    {
        return n * RFactor(n-1);
    }
}

uint64_t IGCD(uint64_t a, uint64_t b)
{
    uint64_t t;

    while (b != 0)
    {
        t = b;
        b = a % b;
        a = t;
    }

    return a;
}

uint64_t RGCD(uint64_t a, uint64_t b)
{
    if (b == 0)
    {
        return a;
    }
    else
    {
        return RGCD(b, a % b);
    }
}

//  Extended Recursive Greatest Common Divisor
int64_t ERGCD (uint64_t a, uint64_t b, int64_t* x, int64_t* y)
{
    // Base Case
    if (b == 0) {
        *x = 1;
        *y = 0;
        return a;
    }
    // Recursively find the gcd
    else {
        int64_t g = ERGCD(b, a % b, x, y);
        int64_t x1 = *x;
        int64_t y1 = *y;

        *x = y1;
        *y = x1 - (a / b) * y1;

        return g;
    }
}

// Iterative Extended Greatest Common Divisor
int64_t EIGCD (uint64_t a, uint64_t b, int64_t* x, int64_t* y)
{
    int64_t x0 = 1;
    int64_t y0 = 0;
    int64_t x1 = 0;
    int64_t y1 = 1;

    while (b != 0) {
        uint64_t quotient = a / b;
        uint64_t remainder = a % b;
        a = b;
        b = remainder;

        int64_t temp = x1;
        x1 = x0 - quotient * x1;
        x0 = temp;

        temp = y1;
        y1 = y0 - quotient * y1;
        y0 = temp;
    }

    *x = x0;
    *y = y0;

    return a;
}

// Recursive Linear Diophantine Equation
struct int64_pair RLDES(uint64_t a, uint64_t b, uint64_t c)
{
    int64_t *x, *y;
    x = malloc(sizeof (int64_t));
    y = malloc(sizeof (int64_t));

    if (a == 0 && b == 0)
    {
        // Infinite solutions
        if (c == 0)
        {
            printf("Infinite Solutions Exist\n");
        }
        // No solution
        else
        {
            printf("No Solution Exists\n");
        }
    }
    int64_t gcd = ERGCD(a, b, x, y);

    // Condition for no solutions exist
    if (c % gcd != 0)
    {
        printf("No Solution Exists\n");
    }

    struct int64_pair result;
    result.x = *x;
    result.y = *y;

    free(x);
    free(y);

    return result;
}

// Iterative Linear Diophantine Equation
struct int64_pair ILDES(uint64_t a, uint64_t b, uint64_t c)
{
    int64_t *x, *y;
    x = malloc(sizeof (int64_t));
    y = malloc(sizeof (int64_t));

    if (a == 0 && b == 0)
    {
        // Infinite solutions
        if (c == 0)
        {
            printf("Infinite Solutions Exist\n");
        }
        // No solution
        else
        {
            printf("No Solution Exists\n");
        }
    }
    int64_t gcd = EIGCD(a, b, x, y);

    // Condition for no solutions exist
    if (c % gcd != 0)
    {
        printf("No Solution Exists\n");
    }

    struct int64_pair result;
    result.x = *x;
    result.y = *y;

    free(x);
    free(y);

    return result;
}