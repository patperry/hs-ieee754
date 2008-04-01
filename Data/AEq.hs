-----------------------------------------------------------------------------
-- |
-- Module     : Data.AEq
-- Copyright  : Copyright (c) 2008, Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : expirimental
--
-- A type class for approximate and exact equalilty comparisons and instances 
-- for common data types.
module Data.AEq
    where

import Data.Int
import Data.Maybe    ( fromMaybe )
import Data.Word
import Data.Complex
import Numeric.IEEE

infix  4  ~==, ~/=

class Eq a => AEq a where
    -- | A reliable way to test if two values are exactly equal.  For floating
    -- point values, this will consider @NaN@ to be identical to @NaN@.
    identical :: a -> a -> Bool
    
    -- | An approximate equality comparison operator.  For @RealFloat@ values, 
    -- @(~==) x y = (eqRel  epsilon x y) || (abs (x - y) < epsilon')@.
    (~==) :: a -> a -> Bool
    
    -- | An approximate inequality comparison operator.  For @RealFloat@ values, 
    -- @(~==) x y = (neqRel  epsilon x y) && (abs (x - y) >= epsilon')@.
    (~/=) :: a -> a -> Bool


instance AEq Float where
    identical = identicalF
    (~==) x y = (eqRel  epsilon x y) || (abs (x - y) < epsilon')
    (~/=) x y = (neqRel epsilon x y) && (abs (x - y) >= epsilon')

instance AEq Double where
    identical = identicalF
    (~==) x y = (eqRel  epsilon x y) || (abs (x - y) < epsilon')
    (~/=) x y = (neqRel epsilon x y) && (abs (x - y) >= epsilon')

instance (RealFloat a, AEq a) => AEq (Complex a) where
    identical (x1 :+ y1) (x2 :+ y2) = (identical x1 x2) && (identical y1 y2)
    (~==)     (x1 :+ y1) (x2 :+ y2) = ((~==) x1 x2) && ((~==) y1 y2)
    (~/=)     (x1 :+ y1) (x2 :+ y2) = ((~/=) x1 x2) && ((~/=) y1 y2)

instance AEq Bool where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance AEq Int where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)

instance AEq Int8 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance AEq Int16 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance AEq Int32 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)

instance AEq Int64 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)

instance AEq Word where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)

instance AEq Word8 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance AEq Word16 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance AEq Word32 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance AEq Word64 where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)

instance AEq () where
    identical = (==)
    (~==)     = (==)
    (~/=)     = (/=)
    
instance (AEq a, AEq b) => AEq (a,b) where
    identical (a1,b1) (a2,b2) = (identical a1 a2) && (identical b1 b2)
    (~==)     (a1,b1) (a2,b2) = ((~==) a1 a2) && ((~==) b1 b2)
    (~/=)     (a1,b1) (a2,b2) = ((~/=) a1 a2) && ((~/=) b1 b2)

instance (AEq a, AEq b, AEq c) => AEq (a,b,c) where
    identical (a1,b1,c1) (a2,b2,c2) = (identical a1 a2) && (identical b1 b2) && (identical c1 c2)
    (~==)     (a1,b1,c1) (a2,b2,c2) = ((~==) a1 a2) && ((~==) b1 b2) && ((~==) c1 c2)
    (~/=)     (a1,b1,c1) (a2,b2,c2) = ((~/=) a1 a2) && ((~/=) b1 b2) && ((~/=) c1 c2)

instance (AEq a, AEq b, AEq c, AEq d) => AEq (a,b,c,d) where
    identical (a1,b1,c1,d1) (a2,b2,c2,d2) = (identical a1 a2) && (identical b1 b2) && (identical c1 c2) && (identical d1 d2)
    (~==)     (a1,b1,c1,d1) (a2,b2,c2,d2) = ((~==) a1 a2) && ((~==) b1 b2) && ((~==) c1 c2) && ((~==) d1 d2)
    (~/=)     (a1,b1,c1,d1) (a2,b2,c2,d2) = ((~/=) a1 a2) && ((~/=) b1 b2) && ((~/=) c1 c2) && ((~/=) d1 d2)

instance (AEq a) => AEq [a] where
    identical xs ys = and $ zipWith identical xs ys
    (~==)     xs ys = and $ zipWith (~==) xs ys
    (~/=)     xs ys = and $ zipWith (~/=) xs ys

instance (AEq a) => AEq (Maybe a) where
    identical x y = fromMaybe True $ do x >>= \x' -> y >>= \y' -> return (identical x' y')
    (~==)     x y = fromMaybe True $ do x >>= \x' -> y >>= \y' -> return ((~==) x' y')
    (~/=)     x y = fromMaybe True $ do x >>= \x' -> y >>= \y' -> return ((~/=) x' y')
    
instance (AEq a, AEq b) => AEq (Either a b) where
    identical (Left a1)  (Left a2)  = identical a1 a2
    identical (Right b1) (Right b2) = identical b1 b2
    identical _ _ = False

    (~==) (Left a1)  (Left a2)  = (~==) a1 a2
    (~==) (Right b1) (Right b2) = (~==) b1 b2
    (~==) _ _ = False

    (~/=) (Left a1)  (Left a2)  = (~/=) a1 a2
    (~/=) (Right b1) (Right b2) = (~/=) b1 b2
    (~/=) _ _ = False
