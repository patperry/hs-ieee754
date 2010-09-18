module Main
    where
        
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.AEq
import Numeric.IEEE

type D = Double
type F = Float

test_maxNum = testGroup "maxNum"
    [ testCase "D1" test_maxNum_D1
    , testCase "D2" test_maxNum_D2
    , testCase "D3" test_maxNum_D3
    , testCase "D4" test_maxNum_D4    
    , testCase "F1" test_maxNum_F1
    , testCase "F2" test_maxNum_F2
    , testCase "F3" test_maxNum_F3
    , testCase "F4" test_maxNum_F4    
    ]

test_maxNum_D1 = maxNum nan 1 @?= (1 :: D)
test_maxNum_D2 = maxNum 1 nan @?= (1 :: D)
test_maxNum_D3 = maxNum 1 0   @?= (1 :: D)
test_maxNum_D4 = maxNum 0 1   @?= (1 :: D)

test_maxNum_F1 = maxNum nan 1 @?= (1 :: F)
test_maxNum_F2 = maxNum 1 nan @?= (1 :: F)
test_maxNum_F3 = maxNum 1 0   @?= (1 :: F)
test_maxNum_F4 = maxNum 0 1   @?= (1 :: F)


test_minNum = testGroup "minNum"
    [ testCase "D1" test_minNum_D1
    , testCase "D2" test_minNum_D2
    , testCase "D3" test_minNum_D3
    , testCase "D4" test_minNum_D4    
    , testCase "F1" test_minNum_F1
    , testCase "F2" test_minNum_F2
    , testCase "F3" test_minNum_F3
    , testCase "F4" test_minNum_F4    
    ]

test_minNum_D1 = minNum nan 1 @?= (1 :: D)
test_minNum_D2 = minNum 1 nan @?= (1 :: D)
test_minNum_D3 = minNum 1 2   @?= (1 :: D)
test_minNum_D4 = minNum 2 1   @?= (1 :: D)

test_minNum_F1 = minNum nan 1 @?= (1 :: F)
test_minNum_F2 = minNum 1 nan @?= (1 :: F)
test_minNum_F3 = minNum 1 2   @?= (1 :: F)
test_minNum_F4 = minNum 2 1   @?= (1 :: F)


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


test_IEEE = testGroup "IEEE"
    [ test_maxNum
    , test_minNum
    , test_nan
    , test_infinity
    , test_succIEEE
    , test_predIEEE
    ]

main :: IO ()
main = defaultMain [ test_IEEE
                   ]
