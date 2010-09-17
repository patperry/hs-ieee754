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

import Foreign.C.Types( CFloat, CDouble )

-- | IEEE floating point numbers.
class (RealFloat a) => IEEE a where
    -- | Infinity value.
    infinity :: a

    -- | NaN value.
    nan :: a

    -- | The smallest nonzero representable normalized value.
    minNormal :: a

    -- | The largest finite representable value.
    maxFinite :: a

    -- | The smallest positive floating-point number @x@ such that @1 + x /= 1@.
    epsilon :: a

    -- | The number of significand bits which are equal in the two arguments
    -- (ported from the Tango math library for D).  The result is between
    -- @0@ and @floatDigits@.
    feqrel :: a -> a -> Int
    
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


instance IEEE Float where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    minNormal = 1.17549435e-38
    {-# INLINE minNormal #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}
    epsilon = 1.19209290e-07
    {-# INLINE epsilon #-}
    feqrel = c_feqrelf
    {-# INLINE feqrel #-}
    expm1 = c_expm1f
    {-# INLINE expm1 #-}
    log1p = c_log1pf
    {-# INLINE log1p #-}


instance IEEE CFloat where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    minNormal = 1.17549435e-38
    {-# INLINE minNormal #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}
    epsilon = 1.19209290e-07
    {-# INLINE epsilon #-}
    feqrel x y = c_feqrelf (realToFrac x) (realToFrac y)
    {-# INLINE feqrel #-}
    expm1 x = realToFrac $ c_expm1f (realToFrac x)
    {-# INLINE expm1 #-}
    log1p x = realToFrac $ c_log1pf (realToFrac x)
    {-# INLINE log1p #-}

instance IEEE Double where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    minNormal = 2.2250738585072014e-308
    {-# INLINE minNormal #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}
    epsilon = 2.2204460492503131e-16
    {-# INLINE epsilon #-}
    feqrel = c_feqrel
    {-# INLINE feqrel #-}
    expm1 = c_expm1
    {-# INLINE expm1 #-}
    log1p = c_log1p
    {-# INLINE log1p #-}

instance IEEE CDouble where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = 0/0
    {-# INLINE nan #-}
    minNormal = 2.2250738585072014e-308
    {-# INLINE minNormal #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}
    epsilon = 2.2204460492503131e-16
    {-# INLINE epsilon #-}
    feqrel x y = c_feqrel (realToFrac x) (realToFrac y)
    {-# INLINE feqrel #-}
    expm1 x = realToFrac $ c_expm1 (realToFrac x)
    {-# INLINE expm1 #-}
    log1p x = realToFrac $ c_log1p (realToFrac x)
    {-# INLINE log1p #-}

foreign import ccall unsafe "feqrel.h feqrel"
    c_feqrel :: Double -> Double -> Int
foreign import ccall unsafe "feqrel.h feqrelf"
    c_feqrelf :: Float -> Float -> Int

foreign import ccall unsafe "math.h expm1"
    c_expm1 :: Double -> Double
foreign import ccall unsafe "math.h expm1f"
    c_expm1f :: Float -> Float

foreign import ccall unsafe "math.h log1p"
    c_log1p :: Double -> Double
foreign import ccall unsafe "math.h log1p"
    c_log1pf :: Float -> Float
