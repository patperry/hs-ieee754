{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.IEEE
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
module Numeric.IEEE (
    IEEE(..),
    ) where

import Data.Word
import Foreign.C.Types( CFloat, CDouble )

-- | IEEE floating point numbers.
class (RealFloat a) => IEEE a where
    -- | Infinity value.
    infinity :: a

    -- | The smallest nonzero representable normalized value.
    minNormal :: a

    -- | The largest finite representable value.
    maxFinite :: a

    -- | The smallest positive floating-point number @x@ such that @1 + x /= 1@.
    epsilon :: a

    -- | Return the next largest IEEE value (@Infinity@ and @NaN@ are
    -- unchanged).
    succIEEE :: a -> a

    -- | Return the next smallest IEEE value (@-Infinity@ and @NaN@ are
    -- unchanged).
    predIEEE :: a -> a

    -- | Given two values with the same sign, return the value halfway
    -- between them on the IEEE number line.  If the signs of the values
    -- differ or either is @NaN@, the value is undefined.
    bisectIEEE :: a -> a -> a

    -- | The number of significand bits which are equal in the two arguments
    -- (equivalent to @feqrel@ from the Tango Math library).  The result is
    -- between @0@ and @'floatDigits'@.
    sameSignificandBits :: a -> a -> Int

    -- | Logically equivalent to @exp x - 1@, but more accurate for small
    -- @x@.
    expm1 :: a -> a

    -- | Logically equivalent to @log (1 + x)@, but more accurate for small
    -- @x@.
    log1p :: a -> a

    -- | Return the maximum of two values; if one value is @NaN@, return the
    -- other.
    maxNum :: a -> a -> a
    maxNum x y | isNaN x   = y
               | otherwise = max x y
    {-# INLINE maxNum #-}

    -- | Return the minimum of two values; if one value is @NaN@, return the
    -- other.
    minNum :: a -> a -> a
    minNum x y | isNaN y   = x
               | otherwise = min x y
    {-# INLINE minNum #-}

    -- | Default @NaN@ value.
    nan :: a

    -- | @NaN@ value with a positive integer payload.  Payload must be
    -- ess than 'maxNaNPayload'.  Beware that while some platforms allow
    -- using @0@ as a payload, this behavior is not portable.
    nanWithPayload :: Word64 -> a

    -- | Maximum @NaN@ payload for type @a@.
    maxNaNPayload :: a -> Word64

    -- | The payload stored in a @NaN@ value.  Undefined if the argument
    -- is not @NaN@.
    nanPayload :: a -> Word64


instance IEEE Float where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    nanWithPayload n = c_mknanf (fromIntegral n)
    {-# INLINE nanWithPayload #-}
    maxNaNPayload _ = 0x003FFFFF
    {-# INLINE maxNaNPayload #-}
    nanPayload x = fromIntegral $ c_getnanf x
    {-# INLINE nanPayload #-}
    minNormal = 1.17549435e-38
    {-# INLINE minNormal #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}
    epsilon = 1.19209290e-07
    {-# INLINE epsilon #-}
    succIEEE = c_nextupf
    {-# INLINE succIEEE #-}
    predIEEE = c_nextdownf
    {-# INLINE predIEEE #-}
    bisectIEEE = c_ieeemeanf
    {-# INLINE bisectIEEE #-}
    sameSignificandBits = c_feqrelf
    {-# INLINE sameSignificandBits #-}
    expm1 = c_expm1f
    {-# INLINE expm1 #-}
    log1p = c_log1pf
    {-# INLINE log1p #-}


instance IEEE CFloat where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    nanWithPayload n = realToFrac $ c_mknanf (fromIntegral n)
    {-# INLINE nanWithPayload #-}
    maxNaNPayload _ = 0x003FFFFF
    {-# INLINE maxNaNPayload #-}
    nanPayload x = fromIntegral $ c_getnanf (realToFrac x)
    {-# INLINE nanPayload #-}
    minNormal = 1.17549435e-38
    {-# INLINE minNormal #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}
    epsilon = 1.19209290e-07
    {-# INLINE epsilon #-}
    succIEEE x = realToFrac $ c_nextupf (realToFrac x)
    {-# INLINE succIEEE #-}
    predIEEE x = realToFrac $ c_nextdownf (realToFrac x)
    {-# INLINE predIEEE #-}
    bisectIEEE x y = realToFrac $ c_ieeemeanf (realToFrac x) (realToFrac y)
    {-# INLINE bisectIEEE #-}
    sameSignificandBits x y = c_feqrelf (realToFrac x) (realToFrac y)
    {-# INLINE sameSignificandBits #-}
    expm1 x = realToFrac $ c_expm1f (realToFrac x)
    {-# INLINE expm1 #-}
    log1p x = realToFrac $ c_log1pf (realToFrac x)
    {-# INLINE log1p #-}

instance IEEE Double where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    nanWithPayload n = c_mknan n
    {-# INLINE nanWithPayload #-}
    maxNaNPayload _ = 0x0007FFFFFFFFFFFF
    {-# INLINE maxNaNPayload #-}
    nanPayload x = c_getnan x
    {-# INLINE nanPayload #-}
    minNormal = 2.2250738585072014e-308
    {-# INLINE minNormal #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}
    epsilon = 2.2204460492503131e-16
    {-# INLINE epsilon #-}
    succIEEE = c_nextup
    {-# INLINE succIEEE #-}
    predIEEE = c_nextdown
    {-# INLINE predIEEE #-}
    bisectIEEE = c_ieeemean
    {-# INLINE bisectIEEE #-}
    sameSignificandBits = c_feqrel
    {-# INLINE sameSignificandBits #-}
    expm1 = c_expm1
    {-# INLINE expm1 #-}
    log1p = c_log1p
    {-# INLINE log1p #-}

instance IEEE CDouble where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    nanWithPayload n = realToFrac $ c_mknan n
    {-# INLINE nanWithPayload #-}
    maxNaNPayload _ = 0x0007FFFFFFFFFFFF
    {-# INLINE maxNaNPayload #-}
    nanPayload x = c_getnan (realToFrac x)
    {-# INLINE nanPayload #-}
    minNormal = 2.2250738585072014e-308
    {-# INLINE minNormal #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}
    epsilon = 2.2204460492503131e-16
    {-# INLINE epsilon #-}
    succIEEE x = realToFrac $ c_nextup (realToFrac x)
    {-# INLINE succIEEE #-}
    predIEEE x = realToFrac $ c_nextdown (realToFrac x)
    {-# INLINE predIEEE #-}
    bisectIEEE x y = realToFrac $ c_ieeemean (realToFrac x) (realToFrac y)
    {-# INLINE bisectIEEE #-}
    sameSignificandBits x y = c_feqrel (realToFrac x) (realToFrac y)
    {-# INLINE sameSignificandBits #-}
    expm1 x = realToFrac $ c_expm1 (realToFrac x)
    {-# INLINE expm1 #-}
    log1p x = realToFrac $ c_log1p (realToFrac x)
    {-# INLINE log1p #-}

foreign import ccall unsafe "feqrel"
    c_feqrel :: Double -> Double -> Int
foreign import ccall unsafe "feqrelf"
    c_feqrelf :: Float -> Float -> Int

foreign import ccall unsafe "nextup"
    c_nextup :: Double -> Double

foreign import ccall unsafe "nextupf"
    c_nextupf :: Float -> Float

foreign import ccall unsafe "nextdown"
    c_nextdown :: Double -> Double

foreign import ccall unsafe "nextdownf"
    c_nextdownf :: Float -> Float

foreign import ccall unsafe "ieeemean"
    c_ieeemean :: Double -> Double -> Double

foreign import ccall unsafe "ieeemeanf"
    c_ieeemeanf :: Float -> Float -> Float

foreign import ccall unsafe "expm1"
    c_expm1 :: Double -> Double
foreign import ccall unsafe "expm1f"
    c_expm1f :: Float -> Float

foreign import ccall unsafe "log1p"
    c_log1p :: Double -> Double
foreign import ccall unsafe "log1pf"
    c_log1pf :: Float -> Float

foreign import ccall unsafe "mknan"
    c_mknan :: Word64 -> Double

foreign import ccall unsafe "getnan"
    c_getnan :: Double -> Word64

foreign import ccall unsafe "mknanf"
    c_mknanf :: Word32 -> Float

foreign import ccall unsafe "getnanf"
    c_getnanf :: Float -> Word32
