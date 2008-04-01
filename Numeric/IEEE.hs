-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.IEEE
-- Copyright  : Copyright (c) 2008, Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : expirimental
--
-- Approximate comparison of floating point numbers based on the
-- algorithm in Section 4.2.2 of Knuth's _Seminumerical Algorithms_
-- and NaN-aware minimum and maximum.
-- 
-- Relative accuracy is measured using an interval of size @2 delta@, 
-- where @delta = 2^k epsilon@, and @k@ is the maximum exponent of @x@ and 
-- @y@.  If x and y lie within this interval, they are considered approximately 
-- equal.
-- 
-- Note that x and y are compared to relative accuracy, so these functions are
-- not suitable for testing whether a value is approximately zero.
--
-- The implementation is based on the GNU Scientific Library implementation, 
-- which is based on the package @fcmp@ by T.C. Belding.
module Numeric.IEEE (
    
    -- * Structural equality
    identicalF,
    
    -- * NaN-aware minimum and maximum
    maxF,
    minF,
    maximumF,
    minimumF,
    
    -- * Relative comparisons
    epsilon,
    epsilon',

    eqRel,
    neqRel,
    ltRel,
    lteRel,
    gtRel,
    gteRel,
    compareRel,
    ) where

import Data.List ( foldl1' )

-- | Equivalent to @x == y || (isNaN x && isNaN y)@.
identicalF :: RealFloat a => a -> a -> Bool
identicalF x y =
    x == y || (isNaN x && isNaN y)
{-# INLINE identicalF #-}

-- | A version of 'max' that returns @NaN@ if either argument is @NaN@.
maxF :: RealFloat a => a -> a -> a
maxF a b
    | isNaN a   = a
    | b < a     = a
    | otherwise = b
{-# INLINE maxF #-}

-- | A version of 'min' that returns @NaN@ if either argument is @NaN@.
minF :: RealFloat a => a -> a -> a
minF a b
    | isNaN a   = a
    | b > a     = a
    | otherwise = b
{-# INLINE minF #-}

-- | Equivalent to @foldl1' maxF@.
maximumF :: RealFloat a => [a] -> a
maximumF = foldl1' maxF 

-- | Equivalent to @foldl1' minF@.
minimumF :: RealFloat a => [a] -> a
minimumF = foldl1' minF 



epsHelp :: RealFloat a => (Int -> Int) -> a
epsHelp = epsHelp' undefined
    where
    epsHelp' :: RealFloat a => a -> (Int -> Int) -> a
    epsHelp' a f =
        let digits = floatDigits a
        in encodeFloat 1 $ f digits

-- | A value suitable for relative comparisons when only half of of the 
-- digits of precision are important.  For @Double@s this value is 
-- @1.4901161193847656e-8@.
epsilon :: RealFloat a => a
epsilon =  epsHelp (\digits -> negate $ digits `div` 2)
{-# INLINE epsilon #-}

-- | The smallest positive floating-point number x such that @1 - x != 1@.
-- Suitable for relative comparisons when all but the least significant digit
-- of precision is important.  For @Double@s this value is 
-- @1.1102230246251565e-16@.
epsilon' :: RealFloat a => a
epsilon' = epsHelp (\digits -> negate $ digits)
{-# INLINE epsilon' #-}

compareRelHelp :: (RealFloat a) => (a -> a -> Bool) -> a -> a -> a -> Bool
compareRelHelp cmp eps x y =
    let e           = max (exponent x) (exponent y)
        (epsM,epsE) = decodeFloat eps
        delta       = encodeFloat epsM (epsE + e)
        diff        = x - y
    in
        abs diff `cmp` delta
{-# INLINE compareRelHelp #-}

-- | @eqRel eps x y@. Relative equality comparator.
-- Returns @False@ if either argument is @NaN@.
eqRel :: (RealFloat a) => a -> a -> a -> Bool
eqRel  = compareRelHelp (\diff delta -> abs diff < delta)
{-# INLINE eqRel  #-}

-- | @neqRel eps x y@. Relative inequality comparator.
-- Returns @False@ if either argument is @NaN@.
neqRel :: (RealFloat a) => a -> a -> a -> Bool
neqRel = compareRelHelp (\diff delta -> abs diff >= delta)
{-# INLINE neqRel #-}

-- | @ltRel eps x y@. Relative less-than comparator.  
-- Returns @False@ if either argument is @NaN@.
ltRel :: (RealFloat a) => a -> a -> a -> Bool
ltRel  = compareRelHelp (\diff delta -> diff < -delta)
{-# INLINE ltRel  #-}

-- | @lteRel eps x y@. Relative less-than-or-equal-to comparator. 
-- Returns @False@ if either argument is @NaN@.
lteRel :: (RealFloat a) => a -> a -> a -> Bool
lteRel = compareRelHelp (\diff delta -> diff <= -delta)
{-# INLINE lteRel #-}

-- | @gtRel eps x y@. Relative greater-than comparator.
-- Returns @False@ if either argument is @NaN@.
gtRel :: (RealFloat a) => a -> a -> a -> Bool
gtRel  = compareRelHelp (\diff delta -> diff > delta)
{-# INLINE gtRel  #-}

-- | @gteRel eps x y@. Relative greater-than-or-equal-to comparator.
-- Returns @False@ if either argument is @NaN@.
gteRel :: (RealFloat a) => a -> a -> a -> Bool
gteRel = compareRelHelp (\diff delta -> diff >= delta)
{-# INLINE gteRel #-}

-- | @compareRel eps x y@ gives an ordering of @x@ and @y@ based on a 
-- relative comparison of accuracy @eps@.  This will call @error@ if either
-- argument is @NaN@.
compareRel :: RealFloat a => a -> a -> a -> Ordering
compareRel eps x y =
    if ltRel eps x y
        then LT
        else if gtRel eps x y
            then GT
            else if eqRel eps x y
                then EQ
                else error $ "NaN comparison"
{-# INLINE compareRel #-}
