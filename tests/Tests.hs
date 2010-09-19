module Main
    where

import Control.Monad( forM_, unless )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.AEq
import Numeric.IEEE

type D = Double
type F = Float

infix 1 @?~=, @?==
    
(@?~=) actual expected = 
    unless (actual ~== expected) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

(@?==) actual expected = 
    unless (actual === expected) (assertFailure msg)
  where
    msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual



test_maxNum = testGroup "maxNum"
    [ testCase "D1" test_maxNum_D1
    , testCase "D2" test_maxNum_D2
    , testCase "D3" test_maxNum_D3
    , testCase "D4" test_maxNum_D4
    , testCase "D5" test_maxNum_D5    
    , testCase "F1" test_maxNum_F1
    , testCase "F2" test_maxNum_F2
    , testCase "F3" test_maxNum_F3
    , testCase "F4" test_maxNum_F4
    , testCase "F5" test_maxNum_F5    
    ]

test_maxNum_D1 = maxNum nan 1 @?= (1 :: D)
test_maxNum_D2 = maxNum 1 nan @?= (1 :: D)
test_maxNum_D3 = maxNum 1 0   @?= (1 :: D)
test_maxNum_D4 = maxNum 0 1   @?= (1 :: D)
test_maxNum_D5 =
    maxNum (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: D)
test_maxNum_F1 = maxNum nan 1 @?= (1 :: F)
test_maxNum_F2 = maxNum 1 nan @?= (1 :: F)
test_maxNum_F3 = maxNum 1 0   @?= (1 :: F)
test_maxNum_F4 = maxNum 0 1   @?= (1 :: F)
test_maxNum_F5 =
    maxNum (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: F)


test_minNum = testGroup "minNum"
    [ testCase "D1" test_minNum_D1
    , testCase "D2" test_minNum_D2
    , testCase "D3" test_minNum_D3
    , testCase "D4" test_minNum_D4
    , testCase "D5" test_minNum_D5    
    , testCase "F1" test_minNum_F1
    , testCase "F2" test_minNum_F2
    , testCase "F3" test_minNum_F3
    , testCase "F4" test_minNum_F4
    , testCase "F5" test_minNum_F5    
    ]

test_minNum_D1 = minNum nan 1 @?= (1 :: D)
test_minNum_D2 = minNum 1 nan @?= (1 :: D)
test_minNum_D3 = minNum 1 2   @?= (1 :: D)
test_minNum_D4 = minNum 2 1   @?= (1 :: D)
test_minNum_D5 =
    minNum (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: D)
test_minNum_F1 = minNum nan 1 @?= (1 :: F)
test_minNum_F2 = minNum 1 nan @?= (1 :: F)
test_minNum_F3 = minNum 1 2   @?= (1 :: F)
test_minNum_F4 = minNum 2 1   @?= (1 :: F)
test_minNum_F5 =
    minNum (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: F)



test_maxNaN = testGroup "maxNaN"
    [ testCase "D1" test_maxNaN_D1
    , testCase "D2" test_maxNaN_D2
    , testCase "D3" test_maxNaN_D3
    , testCase "D4" test_maxNaN_D4
    , testCase "D5" test_maxNaN_D5    
    , testCase "F1" test_maxNaN_F1
    , testCase "F2" test_maxNaN_F2
    , testCase "F3" test_maxNaN_F3
    , testCase "F4" test_maxNaN_F4
    , testCase "F5" test_maxNaN_F5    
    ]

test_maxNaN_D1 = maxNaN nan 1 @?== (nan :: D)
test_maxNaN_D2 = maxNaN 1 nan @?== (nan :: D)
test_maxNaN_D3 = maxNaN 1 0   @?== (1 :: D)
test_maxNaN_D4 = maxNaN 0 1   @?== (1 :: D)
test_maxNaN_D5 =
    maxNaN (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: D)
test_maxNaN_F1 = maxNaN nan 1 @?== (nan :: F)
test_maxNaN_F2 = maxNaN 1 nan @?== (nan :: F)
test_maxNaN_F3 = maxNaN 1 0   @?== (1 :: F)
test_maxNaN_F4 = maxNaN 0 1   @?== (1 :: F)
test_maxNaN_F5 =
    maxNaN (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: F)



test_minNaN = testGroup "minNaN"
    [ testCase "D1" test_minNaN_D1
    , testCase "D2" test_minNaN_D2
    , testCase "D3" test_minNaN_D3
    , testCase "D4" test_minNaN_D4
    , testCase "D5" test_minNaN_D5    
    , testCase "F1" test_minNaN_F1
    , testCase "F2" test_minNaN_F2
    , testCase "F3" test_minNaN_F3
    , testCase "F4" test_minNaN_F4
    , testCase "F5" test_minNaN_F5    
    ]

test_minNaN_D1 = minNaN nan 1 @?== (nan :: D)
test_minNaN_D2 = minNaN 1 nan @?== (nan :: D)
test_minNaN_D3 = minNaN 1 2   @?== (1 :: D)
test_minNaN_D4 = minNaN 2 1   @?== (1 :: D)
test_minNaN_D5 =
    minNaN (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: D)
test_minNaN_F1 = minNaN nan 1 @?== (nan :: F)
test_minNaN_F2 = minNaN 1 nan @?== (nan :: F)
test_minNaN_F3 = minNaN 1 2   @?== (1 :: F)
test_minNaN_F4 = minNaN 2 1   @?== (1 :: F)
test_minNaN_F5 =
    minNaN (nanWithPayload 1) (nanWithPayload 2)
        @?== (nanWithPayload 1 :: F)




test_nan = testGroup "nan" $
    [ testCase "D" test_nan_D
    , testCase "F" test_nan_F
    ]

test_nan_D = isNaN (nan :: D) @?= True
test_nan_F = isNaN (nan :: F) @?= True


test_infinity = testGroup "infinity"
    [ testCase "D1" test_infinity_D1
    , testCase "D2" test_infinity_D2
    , testCase "F1" test_infinity_F1
    , testCase "F2" test_infinity_F2
    ]

test_infinity_D1 = isInfinite (infinity :: D) @?= True
test_infinity_D2 = infinity > (0 :: D) @?= True

test_infinity_F1 = isInfinite (infinity :: F) @?= True
test_infinity_F2 = infinity > (0 :: F) @?= True

-- succIEEE and predIEEE tests ported from tango/math/IEEE.d
test_succIEEE = testGroup "succIEEE"
    [ testCase "nan D" test_succIEEE_nan_D
    , testCase "neg D1" test_succIEEE_neg_D1
    , testCase "neg D2" test_succIEEE_neg_D2
    , testCase "neg D3" test_succIEEE_neg_D3
    , testCase "neg denorm D1" test_succIEEE_neg_denorm_D1
    , testCase "neg denorm D2" test_succIEEE_neg_denorm_D2
    , testCase "neg denrom D3" test_succIEEE_neg_denorm_D3
    , testCase "zero D1" test_succIEEE_zero_D1
    , testCase "zero D2" test_succIEEE_zero_D2
    , testCase "pos denorm D1" test_succIEEE_pos_denorm_D1
    , testCase "pos denorm D2" test_succIEEE_pos_denorm_D2
    , testCase "pos D1" test_succIEEE_pos_D1
    , testCase "pos D2" test_succIEEE_pos_D2
    , testCase "pos D3" test_succIEEE_pos_D3
    , testCase "nan F" test_succIEEE_nan_F
    , testCase "neg F1" test_succIEEE_neg_F1
    , testCase "neg F2" test_succIEEE_neg_F2
    , testCase "neg F3" test_succIEEE_neg_F3
    , testCase "neg denorm F1" test_succIEEE_neg_denorm_F1
    , testCase "neg denorm F2" test_succIEEE_neg_denorm_F2
    , testCase "neg denrom F3" test_succIEEE_neg_denorm_F3
    , testCase "zero F1" test_succIEEE_zero_F1
    , testCase "zero F2" test_succIEEE_zero_F2
    , testCase "pos denorm F1" test_succIEEE_pos_denorm_F1
    , testCase "pos denorm F2" test_succIEEE_pos_denorm_F2
    , testCase "pos F1" test_succIEEE_pos_F1
    , testCase "pos F2" test_succIEEE_pos_F2
    , testCase "pos F3" test_succIEEE_pos_F3
    ]

test_succIEEE_nan_D = isNaN (succIEEE (nan :: D)) @?= True
test_succIEEE_neg_D1 = succIEEE (-infinity) @?= (-maxFinite :: D)
test_succIEEE_neg_D2 = succIEEE (-1 - epsilon) @?= (-1 :: D)
test_succIEEE_neg_D3 = succIEEE (-2) @?= (-2 + epsilon :: D)
test_succIEEE_neg_denorm_D1 = succIEEE (-minNormal) @?= (-minNormal*(1 - epsilon) :: D)
test_succIEEE_neg_denorm_D2 = succIEEE (-minNormal*(1-epsilon)) @?= (-minNormal*(1-2*epsilon) :: D)
test_succIEEE_neg_denorm_D3 = isNegativeZero (succIEEE (-minNormal*epsilon :: D)) @?= True
test_succIEEE_zero_D1 = succIEEE (-0) @?= (minNormal * epsilon :: D)
test_succIEEE_zero_D2 = succIEEE 0 @?= (minNormal * epsilon :: D)
test_succIEEE_pos_denorm_D1 = succIEEE (minNormal*(1-epsilon)) @?= (minNormal :: D)
test_succIEEE_pos_denorm_D2 = succIEEE (minNormal) @?= (minNormal*(1+epsilon) :: D)
test_succIEEE_pos_D1 = succIEEE 1 @?= (1 + epsilon :: D)
test_succIEEE_pos_D2 = succIEEE (2 - epsilon) @?= (2 :: D)
test_succIEEE_pos_D3 = succIEEE maxFinite @?= (infinity :: D)

test_succIEEE_nan_F = isNaN (succIEEE (nan :: F)) @?= True
test_succIEEE_neg_F1 = succIEEE (-infinity) @?= (-maxFinite :: F)
test_succIEEE_neg_F2 = succIEEE (-1 - epsilon) @?= (-1 :: F)
test_succIEEE_neg_F3 = succIEEE (-2) @?= (-2 + epsilon :: F)
test_succIEEE_neg_denorm_F1 = succIEEE (-minNormal) @?= (-minNormal*(1 - epsilon) :: F)
test_succIEEE_neg_denorm_F2 = succIEEE (-minNormal*(1-epsilon)) @?= (-minNormal*(1-2*epsilon) :: F)
test_succIEEE_neg_denorm_F3 = isNegativeZero (succIEEE (-minNormal*epsilon :: F)) @?= True
test_succIEEE_zero_F1 = succIEEE (-0) @?= (minNormal * epsilon :: F)
test_succIEEE_zero_F2 = succIEEE 0 @?= (minNormal * epsilon :: F)
test_succIEEE_pos_denorm_F1 = succIEEE (minNormal*(1-epsilon)) @?= (minNormal :: F)
test_succIEEE_pos_denorm_F2 = succIEEE (minNormal) @?= (minNormal*(1+epsilon) :: F)
test_succIEEE_pos_F1 = succIEEE 1 @?= (1 + epsilon :: F)
test_succIEEE_pos_F2 = succIEEE (2 - epsilon) @?= (2 :: F)
test_succIEEE_pos_F3 = succIEEE maxFinite @?= (infinity :: F)


test_predIEEE = testGroup "predIEEE"
    [ testCase "D" test_predIEEE_D
    , testCase "F" test_predIEEE_F
    ]

test_predIEEE_D = predIEEE (1 + epsilon) @?= (1 :: D)
test_predIEEE_F = predIEEE (1 + epsilon) @?= (1 :: F)

test_bisectIEEE = testGroup "bisectIEEE"
    [ testCase "D1" test_bisectIEEE_D1
    , testCase "D2" test_bisectIEEE_D2
    , testCase "D3" test_bisectIEEE_D3
    , testCase "D4" test_bisectIEEE_D4
    , testCase "D5" test_bisectIEEE_D5            
    , testCase "D6" test_bisectIEEE_D6            
    , testCase "D7" test_bisectIEEE_D7                    
    , testCase "D8" test_bisectIEEE_D8    
    , testCase "D9" test_bisectIEEE_D9
    , testCase "F1" test_bisectIEEE_F1
    , testCase "F2" test_bisectIEEE_F2
    , testCase "F3" test_bisectIEEE_F3
    , testCase "F4" test_bisectIEEE_F4
    , testCase "F5" test_bisectIEEE_F5            
    , testCase "F6" test_bisectIEEE_F6            
    , testCase "F7" test_bisectIEEE_F7                    
    , testCase "F8" test_bisectIEEE_F8    
    , testCase "F9" test_bisectIEEE_F9
    ]

test_bisectIEEE_D1 = bisectIEEE (-0) (-1e-20) < (0 :: D) @?= True
test_bisectIEEE_D2 = bisectIEEE (0) (1e-20) > (0 :: D) @?= True
test_bisectIEEE_D3 = bisectIEEE 1 4 @?= (2 :: D)
test_bisectIEEE_D4 = bisectIEEE (2*1.013) (8*1.013) @?= (4*1.013 :: D)
test_bisectIEEE_D5 = bisectIEEE (-1) (-4) @?= (-2 :: D)
test_bisectIEEE_D6 = bisectIEEE (-1) (-2) @?= (-1.5 :: D)
test_bisectIEEE_D7 =
    bisectIEEE (-1*(1+8*epsilon)) (-2*(1+8*epsilon))
        @?= (-1.5*(1+5*epsilon) :: D)
test_bisectIEEE_D8 =
    bisectIEEE (encodeFloat 0x100000 60) (encodeFloat 0x100000 (-10))
        @?= (encodeFloat 0x100000 25 :: D)
test_bisectIEEE_D9 =
    bisectIEEE 0 infinity @?= (1.5 :: D)

test_bisectIEEE_F1 = bisectIEEE (-0) (-1e-20) < (0 :: F) @?= True
test_bisectIEEE_F2 = bisectIEEE (0) (1e-20) > (0 :: F) @?= True
test_bisectIEEE_F3 = bisectIEEE 1 4 @?= (2 :: F)
test_bisectIEEE_F4 = bisectIEEE (2*1.013) (8*1.013) @?= (4*1.013 :: F)
test_bisectIEEE_F5 = bisectIEEE (-1) (-4) @?= (-2 :: F)
test_bisectIEEE_F6 = bisectIEEE (-1) (-2) @?= (-1.5 :: F)
test_bisectIEEE_F7 =
    bisectIEEE (-1*(1+8*epsilon)) (-2*(1+8*epsilon))
        @?= (-1.5*(1+5*epsilon) :: F)
test_bisectIEEE_F8 =
    bisectIEEE (encodeFloat 0x100000 60) (encodeFloat 0x100000 (-10))
        @?= (encodeFloat 0x100000 25 :: F)
test_bisectIEEE_F9 =
    bisectIEEE 0 infinity @?= (1.5 :: F)


test_sameSignificandBits = testGroup "sameSignificandBits" $
    [ testCase "exact D1" test_sameSignificandBits_exact_D1
    , testCase "exact D2" test_sameSignificandBits_exact_D2
    , testCase "exact D3" test_sameSignificandBits_exact_D3
    , testCase "exact D4" test_sameSignificandBits_exact_D4
    , testCase "fewbits D1" test_sameSignificandBits_fewbits_D1
    , testCase "fewbits D2" test_sameSignificandBits_fewbits_D2
    , testCase "fewbits D3" test_sameSignificandBits_fewbits_D3
    , testCase "fewbits D4" test_sameSignificandBits_fewbits_D4
    , testCase "fewbits D5" test_sameSignificandBits_fewbits_D5
    , testCase "fewbits D6" test_sameSignificandBits_fewbits_D6
    , testCase "fewbits D7" test_sameSignificandBits_fewbits_D7
    , testCase "close D1" test_sameSignificandBits_close_D1
    , testCase "close D2" test_sameSignificandBits_close_D2
    , testCase "close D3" test_sameSignificandBits_close_D3
    , testCase "close D4" test_sameSignificandBits_close_D4
    , testCase "close D5" test_sameSignificandBits_close_D5
    , testCase "2factors D1" test_sameSignificandBits_2factors_D1
    , testCase "2factors D2" test_sameSignificandBits_2factors_D2
    , testCase "2factors D3" test_sameSignificandBits_2factors_D3
    , testCase "2factors D4" test_sameSignificandBits_2factors_D4
    , testCase "extreme D1" test_sameSignificandBits_extreme_D1
    , testCase "extreme D2" test_sameSignificandBits_extreme_D2
    , testCase "extreme D3" test_sameSignificandBits_extreme_D3
    , testCase "extreme D4" test_sameSignificandBits_extreme_D4
    , testCase "extreme D5" test_sameSignificandBits_extreme_D5
    , testCase "extreme D6" test_sameSignificandBits_extreme_D6
    , testCase "exact F1" test_sameSignificandBits_exact_F1
    , testCase "exact F2" test_sameSignificandBits_exact_F2
    , testCase "exact F3" test_sameSignificandBits_exact_F3
    , testCase "exact F4" test_sameSignificandBits_exact_F4
    , testCase "fewbits F1" test_sameSignificandBits_fewbits_F1
    , testCase "fewbits F2" test_sameSignificandBits_fewbits_F2
    , testCase "fewbits F3" test_sameSignificandBits_fewbits_F3
    , testCase "fewbits F4" test_sameSignificandBits_fewbits_F4
    , testCase "fewbits F5" test_sameSignificandBits_fewbits_F5
    , testCase "fewbits F6" test_sameSignificandBits_fewbits_F6
    , testCase "fewbits F7" test_sameSignificandBits_fewbits_F7
    , testCase "close F1" test_sameSignificandBits_close_F1
    , testCase "close F2" test_sameSignificandBits_close_F2
    , testCase "close F3" test_sameSignificandBits_close_F3
    , testCase "close F4" test_sameSignificandBits_close_F4
    , testCase "close F5" test_sameSignificandBits_close_F5
    , testCase "2factors F1" test_sameSignificandBits_2factors_F1
    , testCase "2factors F2" test_sameSignificandBits_2factors_F2
    , testCase "2factors F3" test_sameSignificandBits_2factors_F3
    , testCase "2factors F4" test_sameSignificandBits_2factors_F4
    , testCase "extreme F1" test_sameSignificandBits_extreme_F1
    , testCase "extreme F2" test_sameSignificandBits_extreme_F2
    , testCase "extreme F3" test_sameSignificandBits_extreme_F3
    , testCase "extreme F4" test_sameSignificandBits_extreme_F4
    , testCase "extreme F5" test_sameSignificandBits_extreme_F5
    , testCase "extreme F6" test_sameSignificandBits_extreme_F6
    ]

test_sameSignificandBits_exact_D1 =
    sameSignificandBits (maxFinite :: D) maxFinite
        @?= floatDigits (undefined :: D)
test_sameSignificandBits_exact_D2 =
    sameSignificandBits (0 :: D) 0
        @?= floatDigits (undefined :: D)
test_sameSignificandBits_exact_D3 =
    sameSignificandBits (7.1824 :: D) 7.1824
        @?= floatDigits (undefined :: D)
test_sameSignificandBits_exact_D4 =
    sameSignificandBits (infinity :: D) infinity
        @?= floatDigits (undefined :: D)

test_sameSignificandBits_fewbits_D1 =
    forM_ [ 0..mantDig-1 ] $ \i ->
         sameSignificandBits (1 + 2^^i * epsilon) (1 :: D) @?= mantDig - i - 1
  where
    mantDig = floatDigits (undefined :: D)

test_sameSignificandBits_fewbits_D2 =
    forM_ [ 0..mantDig-3 ] $ \i ->
         sameSignificandBits (1 - 2^^i * epsilon) (1 :: D) @?= mantDig - i - 1
  where
    mantDig = floatDigits (undefined :: D)

test_sameSignificandBits_fewbits_D3 =
    forM_ [ 0..mantDig-1 ] $ \i ->
         sameSignificandBits (1 :: D) (1 + (2^^i - 1) * epsilon)
             @?= mantDig - i
  where
    mantDig = floatDigits (undefined :: D)

test_sameSignificandBits_fewbits_D4 =
    sameSignificandBits (1.5 + epsilon) (1.5 :: D)
        @?= floatDigits (undefined :: D) - 1

test_sameSignificandBits_fewbits_D5 =
    sameSignificandBits (1.5 - epsilon) (1.5 :: D)
        @?= floatDigits (undefined :: D) - 1

test_sameSignificandBits_fewbits_D6 =
    sameSignificandBits (1.5 - epsilon) (1.5 + epsilon :: D)
        @?= floatDigits (undefined :: D) - 2

test_sameSignificandBits_fewbits_D7 =
    sameSignificandBits (minNormal / 8) (minNormal / 17 :: D)
        @?= 3

test_sameSignificandBits_close_D1 =
    sameSignificandBits (encodeFloat 0x1B0000 84) (encodeFloat 0x1B8000 84 :: D)
        @?= 5

test_sameSignificandBits_close_D2 =
    sameSignificandBits (encodeFloat 0x180000 10) (encodeFloat 0x1C0000 10 :: D)
        @?= 2

test_sameSignificandBits_close_D3 =
    sameSignificandBits (1.5 * (1 - epsilon)) (1 :: D) @?= 2

test_sameSignificandBits_close_D4 =
    sameSignificandBits 1.5 (1 :: D) @?= 1

test_sameSignificandBits_close_D5 =
    sameSignificandBits (2 * (1 - epsilon)) (1 :: D) @?= 1

test_sameSignificandBits_2factors_D1 =
    sameSignificandBits maxFinite (infinity :: D) @?= 0

test_sameSignificandBits_2factors_D2 =
    sameSignificandBits (2 * (1 - epsilon)) (1 :: D) @?= 1

test_sameSignificandBits_2factors_D3 =
    sameSignificandBits 1 (2 :: D) @?= 0

test_sameSignificandBits_2factors_D4 =
    sameSignificandBits 4 (1 :: D) @?= 0

test_sameSignificandBits_extreme_D1 =
    sameSignificandBits nan (nan :: D) @?= 0

test_sameSignificandBits_extreme_D2 =
    sameSignificandBits 0 (-nan :: D) @?= 0

test_sameSignificandBits_extreme_D3 =
    sameSignificandBits nan (infinity :: D) @?= 0

test_sameSignificandBits_extreme_D4 =
    sameSignificandBits infinity (-infinity :: D) @?= 0

test_sameSignificandBits_extreme_D5 =
    sameSignificandBits (-maxFinite) (infinity :: D) @?= 0

test_sameSignificandBits_extreme_D6 =
    sameSignificandBits (maxFinite) (-maxFinite :: D) @?= 0

test_sameSignificandBits_exact_F1 =
    sameSignificandBits (maxFinite :: F) maxFinite
        @?= floatDigits (undefined :: F)
test_sameSignificandBits_exact_F2 =
    sameSignificandBits (0 :: F) 0
        @?= floatDigits (undefined :: F)
test_sameSignificandBits_exact_F3 =
    sameSignificandBits (7.1824 :: F) 7.1824
        @?= floatDigits (undefined :: F)
test_sameSignificandBits_exact_F4 =
    sameSignificandBits (infinity :: F) infinity
        @?= floatDigits (undefined :: F)

test_sameSignificandBits_fewbits_F1 =
    forM_ [ 0..mantFig-1 ] $ \i ->
         sameSignificandBits (1 + 2^^i * epsilon) (1 :: F) @?= mantFig - i - 1
  where
    mantFig = floatDigits (undefined :: F)

test_sameSignificandBits_fewbits_F2 =
    forM_ [ 0..mantFig-3 ] $ \i ->
         sameSignificandBits (1 - 2^^i * epsilon) (1 :: F) @?= mantFig - i - 1
  where
    mantFig = floatDigits (undefined :: F)

test_sameSignificandBits_fewbits_F3 =
    forM_ [ 0..mantFig-1 ] $ \i ->
         sameSignificandBits (1 :: F) (1 + (2^^i - 1) * epsilon)
             @?= mantFig - i
  where
    mantFig = floatDigits (undefined :: F)

test_sameSignificandBits_fewbits_F4 =
    sameSignificandBits (1.5 + epsilon) (1.5 :: F)
        @?= floatDigits (undefined :: F) - 1

test_sameSignificandBits_fewbits_F5 =
    sameSignificandBits (1.5 - epsilon) (1.5 :: F)
        @?= floatDigits (undefined :: F) - 1

test_sameSignificandBits_fewbits_F6 =
    sameSignificandBits (1.5 - epsilon) (1.5 + epsilon :: F)
        @?= floatDigits (undefined :: F) - 2

test_sameSignificandBits_fewbits_F7 =
    sameSignificandBits (minNormal / 8) (minNormal / 17 :: F)
        @?= 3

test_sameSignificandBits_close_F1 =
    sameSignificandBits (encodeFloat 0x1B0000 84) (encodeFloat 0x1B8000 84 :: F)
        @?= 5

test_sameSignificandBits_close_F2 =
    sameSignificandBits (encodeFloat 0x180000 10) (encodeFloat 0x1C0000 10 :: F)
        @?= 2

test_sameSignificandBits_close_F3 =
    sameSignificandBits (1.5 * (1 - epsilon)) (1 :: F) @?= 2

test_sameSignificandBits_close_F4 =
    sameSignificandBits 1.5 (1 :: F) @?= 1

test_sameSignificandBits_close_F5 =
    sameSignificandBits (2 * (1 - epsilon)) (1 :: F) @?= 1

test_sameSignificandBits_2factors_F1 =
    sameSignificandBits maxFinite (infinity :: F) @?= 0

test_sameSignificandBits_2factors_F2 =
    sameSignificandBits (2 * (1 - epsilon)) (1 :: F) @?= 1

test_sameSignificandBits_2factors_F3 =
    sameSignificandBits 1 (2 :: F) @?= 0

test_sameSignificandBits_2factors_F4 =
    sameSignificandBits 4 (1 :: F) @?= 0

test_sameSignificandBits_extreme_F1 =
    sameSignificandBits nan (nan :: F) @?= 0

test_sameSignificandBits_extreme_F2 =
    sameSignificandBits 0 (-nan :: F) @?= 0

test_sameSignificandBits_extreme_F3 =
    sameSignificandBits nan (infinity :: F) @?= 0

test_sameSignificandBits_extreme_F4 =
    sameSignificandBits infinity (-infinity :: F) @?= 0

test_sameSignificandBits_extreme_F5 =
    sameSignificandBits (-maxFinite) (infinity :: F) @?= 0

test_sameSignificandBits_extreme_F6 =
    sameSignificandBits (maxFinite) (-maxFinite :: F) @?= 0


test_nanWithPayload = testGroup "nanWithPayload"
    [ testCase "D1" test_nanWithPayload_D1
    , testCase "D2" test_nanWithPayload_D2
    , testCase "F1" test_nanWithPayload_F1
    , testCase "F2" test_nanWithPayload_F2
    ]

test_nanWithPayload_D1 =
    isNaN (nanWithPayload 1 :: D) @?= True

test_nanWithPayload_D2 =
    isNaN (nanWithPayload maxPayload :: D) @?= True
  where
    maxPayload = maxNaNPayload (undefined :: D)

test_nanWithPayload_F1 =
    isNaN (nanWithPayload 1 :: F) @?= True

test_nanWithPayload_F2 =
    isNaN (nanWithPayload maxPayload :: F) @?= True
  where
    maxPayload = maxNaNPayload (undefined :: F)


test_nanPayload = testGroup "nanPayload"
    [ testCase "D1" test_nanPayload_D1
    , testCase "D2" test_nanPayload_D2
    , testCase "D3" test_nanPayload_D3
    , testCase "F1" test_nanPayload_F1
    , testCase "F2" test_nanPayload_F2
    , testCase "F3" test_nanPayload_F3    
    ]

test_nanPayload_D1 =
    nanPayload (nanWithPayload 1 :: D) @?= 1

test_nanPayload_D2 =
    nanPayload (nanWithPayload maxPayload :: D) @?= maxPayload
  where
    maxPayload = maxNaNPayload (undefined :: D)

test_nanPayload_D3 =
    nanPayload (nanWithPayload (maxPayload + 1) :: D) @?= 0
  where
    maxPayload = maxNaNPayload (undefined :: D)

test_nanPayload_F1 =
    nanPayload (nanWithPayload 1 :: F) @?= 1

test_nanPayload_F2 =
    nanPayload (nanWithPayload maxPayload :: F) @?= maxPayload
  where
    maxPayload = maxNaNPayload (undefined :: F)

test_nanPayload_F3 =
    nanPayload (nanWithPayload (maxPayload + 1) :: F) @?= 0
  where
    maxPayload = maxNaNPayload (undefined :: F)

test_copySign = testGroup "copySign"
    [ testCase "D1" test_copySign_D1
    , testCase "D2" test_copySign_D2
    , testCase "D3" test_copySign_D3    
    , testCase "D4" test_copySign_D4    
    , testCase "D5" test_copySign_D5    
    , testCase "D6" test_copySign_D6                    
    , testCase "F1" test_copySign_F1
    , testCase "F2" test_copySign_F2
    , testCase "F3" test_copySign_F3    
    , testCase "F4" test_copySign_F4    
    , testCase "F5" test_copySign_F5    
    , testCase "F6" test_copySign_F6                    
    ]

test_copySign_D1 =
    copySign 0.9 (-1.2) @?= (-0.9 :: D)
test_copySign_D2 =
    copySign 0.9 (1.2) @?= (0.9 :: D)
test_copySign_D3 =
    copySign (-0.9 )(1.2) @?= (0.9 :: D)
test_copySign_D4 =
    copySign (-0.9) (-1.2) @?= (-0.9 :: D)
test_copySign_D5 =
    copySign 1 (copySign nan 1) @?= (1 :: D)
test_copySign_D6 =
    copySign 1 (copySign nan (-1)) @?= (-1 :: D)

test_copySign_F1 =
    copySign 0.9 (-1.2) @?= (-0.9 :: F)
test_copySign_F2 =
    copySign 0.9 (1.2) @?= (0.9 :: F)
test_copySign_F3 =
    copySign (-0.9 )(1.2) @?= (0.9 :: F)
test_copySign_F4 =
    copySign (-0.9) (-1.2) @?= (-0.9 :: F)
test_copySign_F5 =
    copySign 1 (copySign nan 1) @?= (1 :: F)
test_copySign_F6 =
    copySign 1 (copySign nan (-1)) @?= (-1 :: F)



test_IEEE = testGroup "IEEE"
    [ test_infinity
    , test_copySign
    , test_succIEEE
    , test_predIEEE
    , test_bisectIEEE
    , test_sameSignificandBits
    , test_maxNum
    , test_minNum
    , test_maxNaN
    , test_minNaN
    , test_nan
    , test_nanWithPayload
    , test_nanPayload
    ]

main :: IO ()
main = defaultMain [ test_IEEE
                   ]
