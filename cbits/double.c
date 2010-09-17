
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
