/* adapted from Tango version 0.99.9, BSD Licensed
 */


/* Define WORDS_BIGENDIAN on big-endian platforms; otherwise, assume
 * little-endian.
 *
 * Endianness detection modified from http://esr.ibiblio.org/?p=5095 and
 * https://gist.github.com/panzi/6856583 .
 *
 * In the subsequent code we rely on endianness being the same for integers
 * and floats; this is true for modern systems but false in a few historical
 * machines and some old ARM processors; see
 * http://en.wikipedia.org/wiki/Endianness#Floating-point_and_endianness .
 */
#if defined(__BYTE_ORDER__)
/* (1) Try to use the GCC/Clang __BYTE_ORDER__ defines */
#  if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#    define WORDS_BIGENDIAN 1
#  else
#    if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#      undef WORDS_BIGENDIAN
#    else
#      error byte order not supported
#    endif
#  endif
/* (2) Otherwise, try the __BIG_ENDIAN__ and __LITTLE_ENDIAN__ defines */
#elif defined(__BIG_ENDIAN__)
#  define WORDS_BIGENDIAN 1
#elif defined(__LITTLE_ENDIAN__)
#  undef WORDS_BIGENDIAN
#else
/* (3) As a last resort, try platform-specific fallbacks: */
#  if defined(_WIN16) || defined(_WIN32) || defined(_WIN64)
/*   (a) Assume Windows is little endian (http://stackoverflow.com/a/6449581 ) */
#    undef WORDS_BIGENDIAN
#  else
/*   (b) Otherwise, use endian.h */
#    if (defined(__DragonFly__) || defined(__FreeBSD__) || defined(__NetBSD__) \
         || defined(__OpenBSD__))
#      include <sys/endian.h>
#    else
#      include <endian.h>
#    endif
#    if __BYTE_ORDER == __BIG_ENDIAN
#      define WORDS_BIGENDIAN 1
#    elif __BYTE_ORDER == __LITTLE_ENDIAN
#      undef WORDS_BIGENDIAN
#    else
#      error platform not supported: unable to determine endianess
#    endif
#  endif
#endif


/* REAL_EXPMASK is a ushort mask to select the exponent portion (without sign)
 * REAL_SIGNMASK is a ushort mask to select the sign bit.
 * REAL_EXPPOS_SHORT is the index of the exponent when represented as a uint16_t array.
 * REAL_SIGNPOS_BYTE is the index of the sign when represented as a uint8_t array.
 * REAL_RECIP_EPSILON is the value such that 
 *    (smallest_denormal) * REAL_RECIP_EPSILON == REAL_MIN_NORMAL
 */
#define REAL_RECIP_EPSILON      (1 / REAL_EPSILON)

#if REAL_MANT_DIG == 24
# define REAL_EXPMASK            ((uint16_t) 0x7F80)
# define REAL_SIGNMASK           ((uint16_t) 0x8000)
# define REAL_EXPBIAS            ((uint16_t) 0x3F00)
# define REAL_EXPBIAS_INT32      ((uint32_t) 0x7F800000)
# define REAL_MANTISSAMASK_INT32 ((uint32_t) 0x007FFFFF)
# if WORDS_BIGENDIAN
#  define REAL_EXPPOS_INT16 0
# else
#  define REAL_EXPPOS_INT16 1
# endif
#
#elif REAL_MANT_DIG == 53 /* double */
# define REAL_EXPMASK            ((uint16_t) 0x7FF0)
# define REAL_SIGNMASK           ((uint16_t) 0x8000)
# define REAL_EXPBIAS            ((uint16_t) 0x3FE0)
# define REAL_EXPBIAS_INT32      ((uint32_t) 0x7FF00000)
# define REAL_MANTISSAMASK_INT32 ((uint32_t) 0x000FFFFF); /* for the MSB only */
# if WORDS_BIGENDIAN
#  define REAL_EXPPOS_INT16 0
#  define REAL_SIGNPOS_BYTE 0
# else
#  define REAL_EXPPOS_INT16 3
#  define REAL_SIGNPOS_BYTE 7
# endif
#endif

int
FEQREL (REAL x, REAL y)
{
    /* Public Domain. Original Author: Don Clugston, 18 Aug 2005.
     * Ported to C by Patrick Perry, 26 Feb 2010.
     */
    if (x == y) return REAL_MANT_DIG; /* ensure diff!= 0, cope with INF. */

    REAL diff = REAL_ABS(x - y);

    union { REAL r; uint16_t w[sizeof(REAL)/2]; } pa = { x };
    union { REAL r; uint16_t w[sizeof(REAL)/2]; } pb = { y };
    union { REAL r; uint16_t w[sizeof(REAL)/2]; } pd = { diff };    

    /* The difference in abs(exponent) between x or y and abs(x-y)
     * is equal to the number of significand bits of x which are
     * equal to y. If negative, x and y have different exponents.
     * If positive, x and y are equal to 'bitsdiff' bits.
     * AND with 0x7FFF to form the absolute value.
     * To avoid out-by-1 errors, we subtract 1 so it rounds down
     * if the exponents were different. This means 'bitsdiff' is
     * always 1 lower than we want, except that if bitsdiff==0,
     * they could have 0 or 1 bits in common.
     */
#if REAL_MANT_DIG == 53 /* double */
    int bitsdiff = (( ((pa.w[REAL_EXPPOS_INT16] & REAL_EXPMASK)
                     + (pb.w[REAL_EXPPOS_INT16] & REAL_EXPMASK)
                     - ((uint16_t) 0x8000 - REAL_EXPMASK)) >> 1)
                   - (pd.w[REAL_EXPPOS_INT16] & REAL_EXPMASK)) >> 4;
#elif REAL_MANT_DIG == 24 /* float */
    int bitsdiff = (( ((pa.w[REAL_EXPPOS_INT16] & REAL_EXPMASK)
                     + (pb.w[REAL_EXPPOS_INT16] & REAL_EXPMASK)
                     - ((uint16_t) 0x8000 - REAL_EXPMASK)) >> 1)
                    - (pd.w[REAL_EXPPOS_INT16] & REAL_EXPMASK)) >> 7;
#else
# error unsuported floating-point mantissa size
#endif

    if ((pd.w[REAL_EXPPOS_INT16] & REAL_EXPMASK) == 0) {
        /* Difference is denormal
         * For denormals, we need to add the number of zeros that
         * lie at the start of diff's significand.
         * We do this by multiplying by 2^REAL_MANT_DIG
         */
        pd.r *= REAL_RECIP_EPSILON;

#if REAL_MANT_DIG == 53 /* double */
        return (bitsdiff + REAL_MANT_DIG
                - (pd.w[REAL_EXPPOS_INT16] >> 4));
#elif REAL_MANT_DIG == 24 /* float */
        return (bitsdiff + REAL_MANT_DIG
                - (pd.w[REAL_EXPPOS_INT16] >> 7));
#else
# error unsuported floating-point mantissa size
#endif

    }

    if (bitsdiff > 0)
        return bitsdiff + 1; /* add the 1 we subtracted before */

    /* Avoid out-by-1 errors when factor is almost 2. */
    return (bitsdiff == 0
            && !((pa.w[REAL_EXPPOS_INT16]
                  ^ pb.w[REAL_EXPPOS_INT16]) & REAL_EXPMASK)) ? 1 : 0;
}

#undef REAL_RECIP_EPSILON
#undef REAL_EXPMASK
#undef REAL_SIGNMASK
#undef REAL_EXPBIAS
#undef REAL_EXPBIAS_INT32
#undef REAL_MANTISSAMASK_INT32
#undef REAL_EXPPOS_INT16
#undef REAL_SIGNPOS_BYTE
