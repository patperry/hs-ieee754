
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

/* ported from tango/math/IEEE.d */
double
nextup (double x)
{
    uint64_t *ps = (uint64_t *)&x;

    if ((*ps & 0x7FF0000000000000ULL) == 0x7FF0000000000000ULL) {
        /* First, deal with NANs and infinity */
        if (x == -INFINITY) return -REAL_MAX;
        return x; // +INF and NAN are unchanged.
    }
    if (*ps & 0x8000000000000000ULL)  { /* Negative number */
        if (*ps == 0x8000000000000000ULL) { /* it was negative zero */
            *ps = 0x0000000000000001ULL; /* change to smallest subnormal */
            return x;
        }
        --*ps;
    } else { /* Positive number */
        ++*ps;
    }
    return x;
}

/* ported from tango/math/IEEE.d */
double
nextdown (double x)
{
    return -nextup(-x);
}

/* ported from tango/math/IEEE.d */
double
ieeemean (double x, double y)
{
    if (!((x>=0 && y>=0) || (x<=0 && y<=0))) return NAN;
    
    double u;
    
    uint64_t *ul = (uint64_t *)&u;
    uint64_t *xl = (uint64_t *)&x;
    uint64_t *yl = (uint64_t *)&y;
    uint64_t m = ( ((*xl) & 0x7FFFFFFFFFFFFFFFULL)
                 + ((*yl) & 0x7FFFFFFFFFFFFFFFULL) ) >> 1;
    m |= ((*xl) & 0x8000000000000000ULL);
    *ul = m;
    
    return u;
}

double
mknan (uint64_t payload)
{
    double x = NAN;
    uint64_t *ux = (uint64_t *)(&x);
    
    /* get sign, exponent, and quiet bit from NAN */    
    *ux &= 0xFFF8000000000000ULL; 
    
    /* ignore sign, exponent, and quiet bit in payload */
    payload &= 0x0007FFFFFFFFFFFFULL;
    *ux |= payload;

    return x;
}

uint64_t
getnan (double x)
{
    uint64_t payload = *(uint64_t *)(&x);
    
    /* clear ignore sign, exponent, and quiet bit */
    payload &= 0x0007FFFFFFFFFFFFULL;    
    return payload;
}
