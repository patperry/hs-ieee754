{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.AEq
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- A type class for approximate and exact equalilty comparisons and instances
-- for common data types.
module Data.AEq (
    AEq(..),
    ) where

import Foreign
import Foreign.C.Types
import Data.Complex
import Numeric.IEEE

infix 4 ===, ~==

class Eq a => AEq a where
    -- | An exact equality comparison.  For 'IEEE' types, this considers
    -- @NaN@ to be equivalent to @NaN@.
    (===) :: a -> a -> Bool
    (===) = (==)
    {-# INLINE (===) #-}

    -- | An approximate equality comparison operator.
    --
    -- For real 'IEEE' types, two values are approximately equal in the
    -- following cases:
    --
    --   * at least half of their significand bits agree;
    --
    --   * both values are less than 'epsilon';
    --
    --   * both values are @NaN@.
    --
    -- For complex 'IEEE' types, two values are approximately equal in the
    -- followiing cases:
    --
    --   * their magnitudes are approximately equal and the angle between
    --     them is less than @32*@'epsilon';
    --
    --   * both magnitudes are less than 'epsilon';
    --
    --   * both have a NaN real or imaginary part.
    --
    (~==) :: a -> a -> Bool
    (~==) = (==)
    {-# INLINE (~==) #-}

identicalIEEE :: (IEEE a) => a -> a -> Bool
identicalIEEE x y =
    (x == y) || (isNaN x && isNaN y)
{-# INLINE identicalIEEE #-}

approxEqIEEE :: (IEEE a) => a -> a -> Bool
approxEqIEEE x y =
    ( feqrel x y >= d
    || (x < epsilon && y < epsilon)
    || (isNaN x && isNaN y)
    )
  where
    d = (floatDigits x + 1) `div` 2
{-# INLINE approxEqIEEE #-}

identicalComplexIEEE :: (IEEE a) => Complex a -> Complex a -> Bool
identicalComplexIEEE (x1 :+ y1) (x2 :+ y2) =
    (identicalIEEE x1 x2) && (identicalIEEE y1 y2)
{-# INLINE identicalComplexIEEE #-}

approxEqComplexIEEE :: (IEEE a) => Complex a -> Complex a -> Bool
approxEqComplexIEEE z1 z2 = let
    (r1,c1) = polar z1
    (r2,c2) = polar z2
    angle = abs (c1 - c2)
    in ( ( approxEqIEEE r1 r2
         && (angle < 32*epsilon || angle > 2*(pi - 16*epsilon) || isNaN angle)
         )
       || (r1 < epsilon && r2 < epsilon)
       )
{-# INLINE approxEqComplexIEEE #-}

instance AEq Float where
    (===) = identicalIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqIEEE
    {-# INLINE (~==) #-}

instance AEq Double where
    (===) = identicalIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqIEEE
    {-# INLINE (~==) #-}

instance AEq (Complex Float) where
    (===) = identicalComplexIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqComplexIEEE
    {-# INLINE (~==) #-}

instance AEq (Complex Double) where
    (===) = identicalComplexIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqComplexIEEE
    {-# INLINE (~==) #-}

instance AEq CFloat where
    (===) = identicalIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqIEEE
    {-# INLINE (~==) #-}

instance AEq CDouble where
    (===) = identicalIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqIEEE
    {-# INLINE (~==) #-}

instance AEq (Complex CFloat) where
    (===) = identicalComplexIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqComplexIEEE
    {-# INLINE (~==) #-}

instance AEq (Complex CDouble) where
    (===) = identicalComplexIEEE
    {-# INLINE (===) #-}
    (~==) = approxEqComplexIEEE
    {-# INLINE (~==) #-}

instance AEq Bool
instance AEq Char
instance AEq Int
instance AEq Int8
instance AEq Int16
instance AEq Int32
instance AEq Int64
instance AEq Integer
instance AEq Ordering
instance AEq Word
instance AEq Word8
instance AEq Word16
instance AEq Word32
instance AEq Word64
instance AEq ()
instance AEq WordPtr
instance AEq IntPtr
instance AEq (StablePtr a)
instance AEq (Ptr a)
instance AEq (FunPtr a)
instance AEq (ForeignPtr a)
instance AEq CChar
instance AEq CSChar
instance AEq CUChar
instance AEq CShort
instance AEq CUShort
instance AEq CInt
instance AEq CUInt
instance AEq CLong
instance AEq CULong
instance AEq CPtrdiff
instance AEq CSize
instance AEq CWchar
instance AEq CSigAtomic
instance AEq CLLong
instance AEq CULLong
instance AEq CIntPtr
instance AEq CUIntPtr
instance AEq CIntMax
instance AEq CUIntMax
instance AEq CClock
instance AEq CTime

eqListsWith :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqListsWith f (x:xs) (y:ys) = f x y && eqListsWith f xs ys
eqListsWith _ [] [] = True
eqListsWith _ _  _  = False
{-# INLINE eqListsWith #-}

instance (AEq a) => AEq [a] where
    (===) = eqListsWith (===)
    {-# INLINE (===) #-}
    (~==) = eqListsWith (~==)
    {-# INLINE (~==) #-}

instance (AEq a) => AEq (Maybe a) where
    (===) Nothing  Nothing  = True
    (===) (Just x) (Just y) = (===) x y
    (===) _ _ = False
    {-# INLINE (===) #-}

    (~==) Nothing  Nothing  = True
    (~==) (Just x) (Just y) = (~==) x y
    (~==) _ _ = False
    {-# INLINE (~==) #-}

instance (AEq a, AEq b) => AEq (Either a b) where
    (===) (Left a1)  (Left a2)  = (===) a1 a2
    (===) (Right b1) (Right b2) = (===) b1 b2
    (===) _ _ = False
    {-# INLINE (===) #-}

    (~==) (Left a1)  (Left a2)  = (~==) a1 a2
    (~==) (Right b1) (Right b2) = (~==) b1 b2
    (~==) _ _ = False
    {-# INLINE (~==) #-}

instance (AEq a, AEq b) => AEq (a,b) where
    (===) (a1,b1) (a2,b2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1) (a2,b2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c) => AEq (a,b,c) where
    (===) (a1,b1,c1) (a2,b2,c2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1) (a2,b2,c2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d) => AEq (a,b,c,d) where
    (===) (a1,b1,c1,d1) (a2,b2,c2,d2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1) (a2,b2,c2,d2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e) => AEq (a,b,c,d,e) where
    (===) (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f) => AEq (a,b,c,d,e,f) where
    (===) (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g) => AEq (a,b,c,d,e,f,g) where
    (===) (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h) => AEq (a,b,c,d,e,f,g,h) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i) => AEq (a,b,c,d,e,f,g,h,i) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1) (a2,b2,c2,d2,e2,f2,g2,h2,i2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1) (a2,b2,c2,d2,e2,f2,g2,h2,i2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i, AEq j) => AEq (a,b,c,d,e,f,g,h,i,j) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        && ((===) j1 j2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        && ((~==) j1 j2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i, AEq j, AEq k) => AEq (a,b,c,d,e,f,g,h,i,j,k) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        && ((===) j1 j2)
        && ((===) k1 k2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        && ((~==) j1 j2)
        && ((~==) k1 k2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i, AEq j, AEq k, AEq l) => AEq (a,b,c,d,e,f,g,h,i,j,k,l) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        && ((===) j1 j2)
        && ((===) k1 k2)
        && ((===) l1 l2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        && ((~==) j1 j2)
        && ((~==) k1 k2)
        && ((~==) l1 l2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i, AEq j, AEq k, AEq l, AEq m) => AEq (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        && ((===) j1 j2)
        && ((===) k1 k2)
        && ((===) l1 l2)
        && ((===) m1 m2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        && ((~==) j1 j2)
        && ((~==) k1 k2)
        && ((~==) l1 l2)
        && ((~==) m1 m2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i, AEq j, AEq k, AEq l, AEq m, AEq n) => AEq (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        && ((===) j1 j2)
        && ((===) k1 k2)
        && ((===) l1 l2)
        && ((===) m1 m2)
        && ((===) n1 n2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        && ((~==) j1 j2)
        && ((~==) k1 k2)
        && ((~==) l1 l2)
        && ((~==) m1 m2)
        && ((~==) n1 n2)
        )
    {-# INLINE (~==) #-}

instance (AEq a, AEq b, AEq c, AEq d, AEq e, AEq f, AEq g, AEq h, AEq i, AEq j, AEq k, AEq l, AEq m, AEq n, AEq o) => AEq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    (===) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) =
        (  ((===) a1 a2)
        && ((===) b1 b2)
        && ((===) c1 c2)
        && ((===) d1 d2)
        && ((===) e1 e2)
        && ((===) f1 f2)
        && ((===) g1 g2)
        && ((===) h1 h2)
        && ((===) i1 i2)
        && ((===) j1 j2)
        && ((===) k1 k2)
        && ((===) l1 l2)
        && ((===) m1 m2)
        && ((===) n1 n2)
        && ((===) o1 o2)
        )
    {-# INLINE (===) #-}

    (~==) (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1) (a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) =
        (  ((~==) a1 a2)
        && ((~==) b1 b2)
        && ((~==) c1 c2)
        && ((~==) d1 d2)
        && ((~==) e1 e2)
        && ((~==) f1 f2)
        && ((~==) g1 g2)
        && ((~==) h1 h2)
        && ((~==) i1 i2)
        && ((~==) j1 j2)
        && ((~==) k1 k2)
        && ((~==) l1 l2)
        && ((~==) m1 m2)
        && ((~==) n1 n2)
        && ((~==) o1 o2)
        )
    {-# INLINE (~==) #-}