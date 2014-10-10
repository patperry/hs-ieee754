{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.IEEE
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Operations on IEEE floating point numbers.
--
module Numeric.IEEE (
    -- * IEEE type class
    IEEE(..),
    -- * NaN-aware minimum and maximum
    minNum,
    maxNum,
    minNaN,
    maxNaN,
    ) where

import Data.Word
import Foreign.C.Types( CFloat, CDouble )

-- | IEEE floating point types.
class (RealFloat a) => IEEE a where
    -- | Infinity value.
    infinity :: a

    -- | The smallest representable positive normalized value.
    minNormal :: a

    -- | The largest representable finite value.
    maxFinite :: a

    -- | The smallest positive value @x@ such that @1 + x@ is representable.
    epsilon :: a

    -- | @copySign x y@ returns @x@ with its sign changed to @y@'s.
    copySign :: a -> a -> a

    -- | Return 'True' if two values are /exactly/ (bitwise) equal.
    identicalIEEE :: a -> a -> Bool

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

    -- | Default @NaN@ value.
    nan :: a

    -- | Quiet @NaN@ value with a positive integer payload.  Payload must be
    -- less than 'maxNaNPayload'.  Beware that while some platforms allow
    -- using @0@ as a payload, this behavior is not portable.
    nanWithPayload :: Word64 -> a

    -- | Maximum @NaN@ payload for type @a@.
    maxNaNPayload :: a -> Word64

    -- | The payload stored in a @NaN@ value.  Undefined if the argument
    -- is not @NaN@.
    nanPayload :: a -> Word64


-- | Return the maximum of two values; if one value is @NaN@, return the
-- other.  Prefer the first if both values are @NaN@.
maxNum :: (RealFloat a) => a -> a -> a
maxNum x y | x >= y || isNaN y = x
           | otherwise         = y
{-# INLINE maxNum #-}

-- | Return the minimum of two values; if one value is @NaN@, return the
-- other.  Prefer the first if both values are @NaN@.
minNum :: (RealFloat a) => a -> a -> a
minNum x y | x <= y || isNaN y = x
           | otherwise         = y
{-# INLINE minNum #-}

-- | Return the maximum of two values; if one value is @NaN@, return it.
-- Prefer the first if both values are @NaN@.
maxNaN :: (RealFloat a) => a -> a -> a
maxNaN x y | x >= y || isNaN x = x
           | otherwise         = y
{-# INLINE maxNaN #-}

-- | Return the minimum of two values; if one value is @NaN@, return it.
-- Prefer the first if both values are @NaN@.
minNaN :: (RealFloat a) =>  a -> a -> a
minNaN x y | x <= y || isNaN x = x
           | otherwise         = y
{-# INLINE minNaN #-}


instance IEEE Float where
    identicalIEEE x y = c_identicalf x y /= 0
    {-# INLINE identicalIEEE #-}
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
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
    copySign = c_copysignf
    {-# INLINE copySign #-}
    succIEEE = c_nextupf
    {-# INLINE succIEEE #-}
    predIEEE = c_nextdownf
    {-# INLINE predIEEE #-}
    bisectIEEE = c_ieeemeanf
    {-# INLINE bisectIEEE #-}
    sameSignificandBits = c_feqrelf
    {-# INLINE sameSignificandBits #-}


instance IEEE CFloat where
    identicalIEEE x y = c_identicalf (realToFrac x) (realToFrac y) /= 0
    {-# INLINE identicalIEEE #-}
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
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
    copySign x y = realToFrac $ c_copysignf (realToFrac x) (realToFrac y)
    {-# INLINE copySign #-}
    succIEEE x = realToFrac $ c_nextupf (realToFrac x)
    {-# INLINE succIEEE #-}
    predIEEE x = realToFrac $ c_nextdownf (realToFrac x)
    {-# INLINE predIEEE #-}
    bisectIEEE x y = realToFrac $ c_ieeemeanf (realToFrac x) (realToFrac y)
    {-# INLINE bisectIEEE #-}
    sameSignificandBits x y = c_feqrelf (realToFrac x) (realToFrac y)
    {-# INLINE sameSignificandBits #-}


instance IEEE Double where
    identicalIEEE x y = c_identical x y /= 0
    {-# INLINE identicalIEEE #-}
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
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
    copySign = c_copysign
    {-# INLINE copySign #-}
    succIEEE = c_nextup
    {-# INLINE succIEEE #-}
    predIEEE = c_nextdown
    {-# INLINE predIEEE #-}
    bisectIEEE = c_ieeemean
    {-# INLINE bisectIEEE #-}
    sameSignificandBits = c_feqrel
    {-# INLINE sameSignificandBits #-}


instance IEEE CDouble where
    identicalIEEE x y = c_identical (realToFrac x) (realToFrac y) /= 0
    {-# INLINE identicalIEEE #-}
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
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
    copySign x y = realToFrac $ c_copysign (realToFrac x) (realToFrac y)
    {-# INLINE copySign #-}
    predIEEE x = realToFrac $ c_nextdown (realToFrac x)
    {-# INLINE predIEEE #-}
    bisectIEEE x y = realToFrac $ c_ieeemean (realToFrac x) (realToFrac y)
    {-# INLINE bisectIEEE #-}
    sameSignificandBits x y = c_feqrel (realToFrac x) (realToFrac y)
    {-# INLINE sameSignificandBits #-}


foreign import ccall unsafe "identical"
    c_identical :: Double -> Double -> Int

foreign import ccall unsafe "identicalf"
    c_identicalf :: Float -> Float -> Int

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

foreign import ccall unsafe "copysign"
    c_copysign :: Double -> Double -> Double

foreign import ccall unsafe "copysignf"
    c_copysignf :: Float -> Float -> Float

foreign import ccall unsafe "mknan"
    c_mknan :: Word64 -> Double

foreign import ccall unsafe "getnan"
    c_getnan :: Double -> Word64

foreign import ccall unsafe "mknanf"
    c_mknanf :: Word32 -> Float

foreign import ccall unsafe "getnanf"
    c_getnanf :: Float -> Word32
