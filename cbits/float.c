
#include <float.h>
#include <math.h>
#include <stdint.h>

#define REAL             float
#define REAL_ABS         fabsf

#define REAL_MIN_NORMAL  FLT_MIN
#define REAL_EPSILON     FLT_EPSILON
#define REAL_MAX         FLT_MAX
#define REAL_MANT_DIG    FLT_MANT_DIG

#define FEQREL           feqrelf
#include "feqrel_source.c"

int
identicalf (float x, float y)
{
    uint32_t *ux = (uint32_t *)(&x);
    uint32_t *uy = (uint32_t *)(&y);
    return *ux == *uy;
}

/* ported from tango/math/IEEE.d */
float
nextupf (float x)
{
    uint32_t *ps = (uint32_t *)&x;

    if ((*ps & 0x7F800000) == 0x7F800000) {
        /* First, deal with NANs and infinity */
        if (x == -INFINITY) return -REAL_MAX;
        return x; /* +INF and NAN are unchanged. */
    }
    if (*ps & 0x80000000)  { /* Negative number */
        if (*ps == 0x80000000) { /* it was negative zero */
            *ps = 0x00000001; /* change to smallest subnormal */
            return x;
        }
        --*ps;
    } else { /* Positive number */
        ++*ps;
    }
    return x;
}

/* ported from tango/math/IEEE.d */
float
nextdownf (float x)
{
    return -nextupf(-x);
}

/* ported from tango/math/IEEE.d */
float
ieeemeanf (float x, float y)
{
    if (!((x>=0 && y>=0) || (x<=0 && y<=0))) return NAN;
    
    float u;
    
    uint32_t *ul = (uint32_t *)&u;
    uint32_t *xl = (uint32_t *)&x;
    uint32_t *yl = (uint32_t *)&y;
    uint32_t m = (((*xl) & 0x7FFFFFFF) + ((*yl) & 0x7FFFFFFF)) >> 1;
    m |= ((*xl) & 0x80000000);
    *ul = m;
    
    return u;
}

float
mknanf (uint32_t payload)
{
    float x = NAN;
    uint32_t *ux = (uint32_t *)(&x);
    
    /* get sign, exponent, and quiet bit from NAN */    
    *ux &= 0xFFC00000; 
    
    /* ignore sign, exponent, and quiet bit in payload */
    payload &= 0x003FFFFF;
    *ux |= payload;

    return x;
}

uint32_t
getnanf (float x)
{
    uint32_t payload = *(uint32_t *)(&x);
    
    /* clear sign, exponent, and quiet bit */
    payload &= 0x003FFFFF;
    return payload;
}
