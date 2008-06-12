-----------------------------------------------------------------------------
-- |
-- Module     : Data.AEq
-- Copyright  : Copyright (c) 2008, Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- A type class for approximate and exact equalilty comparisons and instances 
-- for common data types.  
module Data.AEq
    where

import Data.Int
import Data.Word
import Data.Complex
import Numeric.IEEE

infix  4  ===, ~==

class Eq a => AEq a where
    -- | A reliable way to test if two values are exactly equal.  For floating
    -- point values, this will consider @NaN@ to be (===) to @NaN@.
    (===) :: a -> a -> Bool
    
    -- | An approximate equality comparison operator.  For @RealFloat@ values, 
    -- @(~==) x y =   (x == y)
    --             || (abs (x - y) < epsilon) 
    --             || (eqRel delta x y) 
    --             || (isNaN x && isNaN y)@.
    -- For Complex numbers, the if the real and imaginary parts are not
    -- approximately equal, the polar forms are compared, instead.
    (~==) :: a -> a -> Bool
    

instance AEq Float where
    (===) x y =
        (x == y) || (isNaN x && isNaN y)
    (~==) x y =   
        (x == y) || (abs (x - y) < epsilon) || (eqRel delta x y) || (isNaN x && isNaN y)

instance AEq Double where
    (===) x y =
        (x == y) || (isNaN x && isNaN y)
    (~==) x y = 
        (x == y) || (abs (x - y) < epsilon) || (eqRel delta x y) || (isNaN x && isNaN y)

instance (RealFloat a, AEq a) => AEq (Complex a) where
    (===) (x1 :+ y1) (x2 :+ y2) = ((===) x1 x2) && ((===) y1 y2)
    (~==) z1@(x1 :+ y1) z2@(x2 :+ y2) = 
        let (r1,c1) = polar z1
            (r2,c2) = polar z2
            c  = min c1 c2
            c' = max c1 c2
        in (x1 ~== x2 && y1 ~== y2) || (r1 ~== r2) && ((c1 ~== c2) || (c + 2 * pi ~== c'))

instance AEq Bool where
    (===) = (==)
    (~==) = (==)
    
instance AEq Int where
    (===) = (==)
    (~==) = (==)

instance AEq Int8 where
    (===) = (==)
    (~==) = (==)
    
instance AEq Int16 where
    (===) = (==)
    (~==) = (==)
    
instance AEq Int32 where
    (===) = (==)
    (~==) = (==)

instance AEq Int64 where
    (===) = (==)
    (~==) = (==)

instance AEq Word where
    (===) = (==)
    (~==) = (==)

instance AEq Word8 where
    (===) = (==)
    (~==) = (==)
    
instance AEq Word16 where
    (===) = (==)
    (~==) = (==)
    
instance AEq Word32 where
    (===) = (==)
    (~==) = (==)
    
instance AEq Word64 where
    (===) = (==)
    (~==) = (==)

instance AEq () where
    (===) = (==)
    (~==) = (==)
    
instance (AEq a, AEq b) => AEq (a,b) where
    (===) (a1,b1) (a2,b2) = ((===) a1 a2) && ((===) b1 b2)
    (~==) (a1,b1) (a2,b2) = ((~==) a1 a2) && ((~==) b1 b2)

instance (AEq a, AEq b, AEq c) => AEq (a,b,c) where
    (===) (a1,b1,c1) (a2,b2,c2) = ((===) a1 a2) && ((===) b1 b2) && ((===) c1 c2)
    (~==) (a1,b1,c1) (a2,b2,c2) = ((~==) a1 a2) && ((~==) b1 b2) && ((~==) c1 c2)

instance (AEq a, AEq b, AEq c, AEq d) => AEq (a,b,c,d) where
    (===) (a1,b1,c1,d1) (a2,b2,c2,d2) = ((===) a1 a2) && ((===) b1 b2) && ((===) c1 c2) && ((===) d1 d2)
    (~==) (a1,b1,c1,d1) (a2,b2,c2,d2) = ((~==) a1 a2) && ((~==) b1 b2) && ((~==) c1 c2) && ((~==) d1 d2)

instance (AEq a) => AEq [a] where
    (===) xs ys = and $ zipWith (===) xs ys
    (~==) xs ys = and $ zipWith (~==) xs ys

instance (AEq a) => AEq (Maybe a) where
    (===) Nothing  Nothing  = True
    (===) (Just x) (Just y) = (===) x y
    (===) _ _ = False
    
    (~==) Nothing  Nothing  = True
    (~==) (Just x) (Just y) = (~==) x y
    (~==) _ _ = False
    
instance (AEq a, AEq b) => AEq (Either a b) where
    (===) (Left a1)  (Left a2)  = (===) a1 a2
    (===) (Right b1) (Right b2) = (===) b1 b2
    (===) _ _ = False

    (~==) (Left a1)  (Left a2)  = (~==) a1 a2
    (~==) (Right b1) (Right b2) = (~==) b1 b2
    (~==) _ _ = False
