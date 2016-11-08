
#include <float.h>
#include <math.h>
#include <stdint.h>

#define REAL             double
#define REAL_ABS         fabs

#define REAL_MIN_NORMAL  DBL_MIN
#define REAL_EPSILON     DBL_EPSILON
#define REAL_MAX         DBL_MAX
#define REAL_MANT_DIG    DBL_MANT_DIG

#define FEQREL           feqrel
#include "feqrel_source.c"

union double_t {
    double d;
    uint64_t w;
};

int
identical (double x, double y)
{
    union double_t ux = { x };
    union double_t uy = { y };
    return ux.w == uy.w;
}

double
copysign (double x, double y)
{
    union double_t ux = { x };
    union double_t uy = { y };
    union double_t uz;
    uint64_t val  = ux.w & 0x7FFFFFFFFFFFFFFFULL;
    uint64_t sign = uy.w & 0x8000000000000000ULL;
    uz.w = sign | val;
    return uz.d;
}

/* ported from tango/math/IEEE.d nextup */
double
ieeesucc (double x)
{
    union double_t ps = { x };

    if ((ps.w & 0x7FF0000000000000ULL) == 0x7FF0000000000000ULL) {
        /* First, deal with NANs and infinity */
        if (x == -INFINITY) return -REAL_MAX;
        return x; // +INF and NAN are unchanged.
    }
    if (ps.w & 0x8000000000000000ULL)  { /* Negative number */
        if (ps.w == 0x8000000000000000ULL) { /* it was negative zero */
            ps.w = 0x0000000000000001ULL; /* change to smallest subnormal */
            return ps.d;
        }
        --ps.w;
    } else { /* Positive number */
        ++ps.w;
    }
    return ps.d;
}

/* ported from tango/math/IEEE.d nextdown */
double
ieeepred (double x)
{
    return -ieeesucc(-x);
}

/* ported from tango/math/IEEE.d */
double
ieeemean (double x, double y)
{
    if (!((x>=0 && y>=0) || (x<=0 && y<=0))) return NAN;
    
    union double_t ul;
    union double_t xl = { x };
    union double_t yl = { y };
    uint64_t m = ( (xl.w & 0x7FFFFFFFFFFFFFFFULL)
                 + (yl.w & 0x7FFFFFFFFFFFFFFFULL) ) >> 1;
    m |= (xl.w & 0x8000000000000000ULL);
    ul.w = m;
    
    return ul.d;
}

double
mknan (uint64_t payload)
{
    union double_t x = { NAN };
    
    /* get sign, exponent, and quiet bit from NAN */    
    x.w &= 0xFFF8000000000000ULL; 
    
    /* ignore sign, exponent, and quiet bit in payload */
    payload &= 0x0007FFFFFFFFFFFFULL;
    x.w |= payload;

    return x.d;
}

uint64_t
getnan (double x)
{
    union double_t payload = { x };
    
    /* clear sign, exponent, and quiet bit */
    payload.w &= 0x0007FFFFFFFFFFFFULL;    
    return payload.w;
}
