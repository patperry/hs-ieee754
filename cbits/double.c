
#include <float.h>
#include <math.h>
#include <stdint.h>

#define REAL             double
#define REAL_ABS         fabs

#define REAL_MIN_NORMAL  DBL_MIN
#define REAL_EPSILON     DBL_EPSILON
#define REAL_MAX         DBL_MAX
#define REAL_MANT_DIG    DBL_MANT_DIG
#define REAL_NEGINF

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
