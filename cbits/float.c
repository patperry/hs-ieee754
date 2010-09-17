
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
