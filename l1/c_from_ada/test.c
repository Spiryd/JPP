#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include "wrapper.h"

void cmpPairs(struct int64_pair myResult1, struct int64_pair myResult2) {
    assert(myResult1.x == myResult2.x);
    assert(myResult1.y == myResult2.y);
}

int main() 
{
    printf("Factorials are computed by iteration\n");
    assert( IFactor(0) == 1 );
    assert( IFactor(1) == 1 );
    assert( IFactor(2) == 2 );
    assert( IFactor(3) == 6 );
    assert( IFactor(10) == 3628800 );

    printf( "Factorials are computed by recursion\n");
        assert( RFactor(0) == 1 );
        assert( RFactor(1) == 1 );
        assert( RFactor(2) == 2 );
        assert( RFactor(3) == 6 );
        assert( RFactor(10) == 3628800 );


    printf( "Greatest common divisior is computed by iteration\n" );
        assert( IGCD(1, 1) == 1 );
        assert( IGCD(10, 100) == 10 );


    printf( "Greatest common divisior is computed by recursion\n");
        assert( RGCD(1, 1) == 1 );
        assert( RGCD(10, 100) == 10 );


    uint64_t a, b, c;
    struct int64_pair exp_out;

    printf("Normal RLDES tests\n");

        a = 24;
        b = 36;
        c = 12;
        exp_out.x = -1;
        exp_out.y =  1;
        struct int64_pair result = RLDES(a, b, c);
        printf("result.x = %ld\n", result.x);
        printf("result.y = %ld\n", result.y);
        cmpPairs(RLDES(a, b, c), exp_out);

        a = 91;
        b = 35;
        c = 7;
        exp_out.x =  2;
        exp_out.y = -5;
        struct int64_pair result2 = RLDES(a, b, c);
        printf("result2.x = %ld\n", result2.x);
        printf("result2.y = %ld\n", result2.y);
        cmpPairs(RLDES(a, b, c), exp_out);


    printf("Normal ILDES tests\n");

        a = 24;
        b = 36;
        c = 12;
        exp_out.x = -1;
        exp_out.y =  1;
        struct int64_pair result3 = ILDES(a, b, c);
        printf("result3.x = %ld\n", result3.x);
        printf("result3.y = %ld\n", result3.y);
        cmpPairs(ILDES(a, b, c), exp_out);

        a = 91;
        b = 35;
        c = 7;
        exp_out.x =  2;
        exp_out.y = -5;
        cmpPairs(ILDES(a, b, c), exp_out);
}