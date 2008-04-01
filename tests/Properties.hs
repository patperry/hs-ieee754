{-# OPTIONS -fglasgow-exts #-}

import Test.QuickCheck
import Data.Int

import Numeric.IEEE

type D = Double

b = floatRadix (undefined :: D)

nan :: RealFloat a => a
nan = 0 / 0

incSignif :: RealFloat a => Integer -> a -> a
incSignif i x =
    let (m,n) = decodeFloat x
    in encodeFloat (m+i) n


------------------------- NaN-aware min and max -----------------------------

prop_identicalF_nan =
    nan `identicalF` nan

prop_identicalF (x :: D) =
    x `identicalF` x


prop_minF (x :: D) (y :: D) =
    not (isNaN x || isNaN y) ==> minF x y == min x y

prop_minF_nan1 (x :: D) =
    isNaN (minF x nan)
    
prop_minF_nan2 (x :: D) =
    isNaN (minF nan x)


prop_maxF (x :: D) (y :: D) =
    not (isNaN x || isNaN y) ==> maxF x y == max x y

prop_maxF_nan1 (x :: D) =
    isNaN (maxF x nan)
    
prop_maxF_nan2 (x :: D) =
    isNaN (maxF nan x)
    
----------------------------- NaN comparisons -------------------------------

prop_nan1 (x :: D) (eps :: D) =
    eps >= 0 ==> (not . or) [ eqRel  eps x nan
                            , neqRel eps x nan
                            , ltRel  eps x nan
                            , lteRel eps x nan
                            , gtRel  eps x nan
                            , gteRel eps x nan
                            ]

prop_nan2 (x :: D) (eps :: D) =
    eps >= 0 ==> (not . or) [ eqRel  eps nan x
                            , neqRel eps nan x
                            , ltRel  eps nan x
                            , lteRel eps nan x
                            , gtRel  eps nan x
                            , gteRel eps nan x
                            ]


--------------------- Comparisons relative to 0 -----------------------------

prop_0_exact (x :: D) =
    not (isNaN x || x == 0) ==> and [ eqRel  0 x x'
                                    , lteRel 0 x x'
                                    , gteRel 0 x x'
                                    ]
                      && (not . or) [ neqRel 0 x x'
                                    , ltRel  0 x x'
                                    , gtRel  0 x x'
                                    ]
                      && (compareRel epsilon' x x' == EQ)
    where 
        x' = x

prop_plus_1_exact (x :: D) =
    not (isNaN x || x == 0) ==> and [ neqRel 0 x x'
                                    , ltRel  0 x x'
                                    , lteRel 0 x x'
                                    ]
                      && (not . or) [ eqRel  0 x x'
                                    , gtRel  0 x x'
                                    , gteRel 0 x x'
                                    ]
                      && (compareRel 0 x x' == LT)
    where 
        x' = incSignif 1 x

prop_minus_1_exact (x :: D) =
    not (isNaN x || x == 0) ==> and [ neqRel 0 x x'
                                    , gtRel  0 x x'
                                    , gteRel 0 x x'
                                    ]
                      && (not . or) [ eqRel  0 x x'
                                    , ltRel  0 x x'
                                    , lteRel 0 x x'
                                    ]
                      && (compareRel 0 x x' == GT)
    where 
        x' = incSignif (-1) x

--------------------- Comparisons relative to epsilon' ----------------------

prop_plus_1' (x :: D) =
    not (isNaN x || x == 0) ==> and [ eqRel  epsilon' x x'
                                    , lteRel epsilon' x x'
                                    , gteRel epsilon' x x'
                                    ]
                      && (not . or) [ neqRel epsilon' x x'
                                    , ltRel  epsilon' x x'
                                    , gtRel  epsilon' x x'
                                    ]
                      && (compareRel epsilon' x x' == EQ)
    where 
        x' = incSignif 1 x

prop_minus_1' (x :: D) =
    not (isNaN x || x == 0) ==> and [ eqRel  epsilon' x x'
                                    , lteRel epsilon' x x'
                                    , gteRel epsilon' x x'
                                    ]
                      && (not . or) [ neqRel epsilon' x x'
                                    , ltRel  epsilon' x x'
                                    , gtRel  epsilon' x x'
                                    ]
                      && (compareRel epsilon' x x' == EQ)
      where 
        x' = incSignif (-1) x


prop_plus_b' (x :: D) =
    not (isNaN x || x == 0) ==> and [ neqRel  epsilon' x x'
                                    , ltRel   epsilon' x x'
                                    , lteRel  epsilon' x x'
                                    ]
                      && (not . or) [ eqRel  epsilon' x x'
                                    , gtRel  epsilon' x x'
                                    , gteRel epsilon' x x'
                                    ]
                      && (compareRel epsilon' x x' == LT)
      where 
        x' = incSignif b x

prop_minus_b' (x :: D) =
    not (isNaN x || x == 0) ==> and [ neqRel  epsilon' x x'
                                    , gtRel  epsilon' x x'
                                    , gteRel epsilon' x x'
                                    ]
                      && (not . or) [ eqRel  epsilon' x x'
                                    , ltRel   epsilon' x x'
                                    , lteRel  epsilon' x x'
                                    ]
                      && (compareRel epsilon' x x' == GT)
    where 
        x' = incSignif (-b) x


--------------------- Comparisons relative to epsilon -----------------------

prop_plus_b (x :: D) =
    not (isNaN x || x == 0) ==> and [ eqRel  epsilon x x'
                                    , lteRel epsilon x x'
                                    , gteRel epsilon x x'
                                    ]
                      && (not . or) [ neqRel epsilon x x'
                                    , ltRel  epsilon x x'
                                    , gtRel  epsilon x x'
                                    ]
                      && (compareRel epsilon x x' == EQ)
    where 
        x' = incSignif b x

prop_minus_b (x :: D) =
    not (isNaN x || x == 0) ==> and [ eqRel  epsilon x x'
                                    , lteRel epsilon x x'
                                    , gteRel epsilon x x'
                                    ]
                      && (not . or) [ neqRel epsilon x x'
                                    , ltRel  epsilon x x'
                                    , gtRel  epsilon x x'
                                    ]
                      && (compareRel epsilon x x' == EQ)
    where 
        x' = incSignif (-b) x


prop_plus_b1 (x :: D) =
    not (isNaN x || x == 0) ==> and [ neqRel epsilon x x'
                                    , ltRel  epsilon x x'
                                    , lteRel epsilon x x'
                                    ]
                      && (not . or) [ eqRel  epsilon x x'
                                    , gtRel  epsilon x x'
                                    , gteRel epsilon x x'
                                    ]
                      && (compareRel epsilon x x' == LT)
      where 
        x' = incSignif (b+1) x

prop_minus_b1 (x :: D) =
    not (isNaN x || x == 0) ==> and [ neqRel epsilon x x'
                                    , gtRel  epsilon x x'
                                    , gteRel epsilon x x'
                                    ]
                      && (not . or) [ eqRel  epsilon x x'
                                    , ltRel  epsilon x x'
                                    , lteRel epsilon x x'
                                    ]
                      && (compareRel epsilon x x' == GT)
    where 
        x' = incSignif (-b-1) x

main = do
    let runT s a = do print s; a

    runT "identicalF" $ do
        quickCheck prop_identicalF_nan
        quickCheck prop_identicalF

    runT "minF" $ do
        quickCheck prop_minF 
        quickCheck prop_minF_nan1
        quickCheck prop_minF_nan2
    
    runT "maxF" $ do
        quickCheck prop_maxF
        quickCheck prop_maxF_nan1 
        quickCheck prop_maxF_nan2

    runT "NaN comparisons" $ do
        quickCheck prop_nan1
        quickCheck prop_nan2
    
    runT "comparisons relative to 0" $ do
        quickCheck prop_0_exact
        quickCheck prop_plus_1_exact
        quickCheck prop_minus_1_exact
        
    runT "comparisons relative to epsilon'" $ do
        quickCheck prop_plus_1'
        quickCheck prop_minus_1'
        quickCheck prop_plus_b'
        quickCheck prop_minus_b'

    runT "comparisons relative to epsilon" $ do
        quickCheck prop_plus_b
        quickCheck prop_minus_b
        quickCheck prop_plus_b1
        quickCheck prop_minus_b1
        