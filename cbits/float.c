
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
