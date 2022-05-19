import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Control.Monad

import           Data.Interpretation
import           Data.MLSentence
import           Data.PLSentence

instance Arbitrary a => Arbitrary (PLSentence a) where
    arbitrary = sized s
        where s 0 = liftM PLAtomic QC.arbitrary
              s n | n > 0 =
                  let s' = s (n `div` 2)
                  in QC.oneof [ liftM PLAtomic QC.arbitrary
                              , liftM PLNot s'
                              , liftM2 PLOr s' s'
                              , liftM2 PLAnd s' s'
                              , liftM2 PLIf s' s'
                              , liftM2 PLIff s' s'
                              ]
    shrink (PLAtomic _) = []
    shrink (PLNot s) = [s] ++ shrink s
    shrink (PLOr s1 s2) = [s1, s2] ++ [ PLOr s1' s2' | (s1', s2') <- shrink (s1, s2) ]
    shrink (PLAnd s1 s2) = [s1, s2] ++ [ PLAnd s1' s2' | (s1', s2') <- shrink (s1, s2) ]
    shrink (PLIf s1 s2) = [s1, s2] ++ [ PLIf s1' s2' | (s1', s2') <- shrink (s1, s2) ]
    shrink (PLIff s1 s2) = [s1, s2] ++ [ PLIff s1' s2' | (s1', s2') <- shrink (s1, s2) ]

-- Definition of a valuation.
-- Since the PLSentency is arbitrary generated, this definition is sufficient for most cases.
-- Indeed, the probability of Atomic sentences evaluated to True will be roughly 0.5.
valuation :: Interpretation Int
valuation = (> 0)

{-
    Main Tests
-}

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [task1, task2, task4]

{- Task 1 -}

task1 :: TestTree
task1 = testGroup "plSize" [plSizeUnitTest, plSizeQcProps]

plSizeUnitTest = testGroup "plSize Unit tests" [
      testCase "Atomic case" $ plSize (PLAtomic 0) @?= 1
    ]

propNotPlSize :: PLSentence Int -> Bool
propNotPlSize s =  plSize (PLNot s) == 1 + plSize s

propOrPlSize :: PLSentence Int -> PLSentence Int -> Bool
propOrPlSize s1 s2 =  plSize (PLOr s1 s2) == 1 + plSize s1 + plSize s2

plSizeQcProps = testGroup "plSize Quickcheck"
  [ QC.testProperty "PLNot is correct" propNotPlSize
  , QC.testProperty "PLOr is correct" propOrPlSize
  ]

{- Task2 -}

task2 :: TestTree
task2 = testGroup "evaluate properties" [
    QC.testProperty "evaluate i (not x) == not (evaluate i x)" propNotEvaluate
  , QC.testProperty "evaluate i (True or x) == True" propOrTrueEvaluate
  , QC.testProperty "evaluate i (False or x) == evaluate i x" propOrTrueEvaluate
  , QC.testProperty "evaluate i ((x or y) or z) == evaluate i (x (y or z))" propOrAssociativityEvaluate
  , QC.testProperty "evaluate i (x or y) == evaluate i (y or x)" propOrCommutativityEvaluate
  , QC.testProperty "evaluate i (not (x and y)) == evaluate i ((not x) or (nor y))" propAndOrEvaluate
  , QC.testProperty "evaluate i (x implies y) == evaluate i ((not x) or y)" propImpliesEvaluate
  , QC.testProperty "evaluate i (x iff y) == evaluate i ((x implies y) and (y implies x))" propIffEvaluate
  ]


propNotEvaluate :: PLSentence Int -> Bool
propNotEvaluate s = evaluate valuation s' == not (evaluate valuation s)
    where s' = PLNot s

propOrTrueEvaluate :: PLSentence Int -> Bool
propOrTrueEvaluate s = evaluate valuation (PLOr (PLAtomic 1) s) == True
-- Note : valuation of PLAtomic 1 is True

propOrFalseEvaluate :: PLSentence Int -> Bool
propOrFalseEvaluate s = evaluate valuation (PLOr (PLAtomic 0) s) == evaluate valuation s
-- Note : valuation of PLAtomic 0 is False

propOrAssociativityEvaluate :: PLSentence Int -> PLSentence Int -> PLSentence Int -> Bool
propOrAssociativityEvaluate s1 s2 s3 = evaluate valuation s1' == evaluate valuation s2'
    where s1' = PLOr (PLOr s1 s2) s3
          s2' = PLOr s1 $ PLOr s2 s3

propOrCommutativityEvaluate :: PLSentence Int -> PLSentence Int -> Bool
propOrCommutativityEvaluate s1 s2 = evaluate valuation s1' == evaluate valuation s2'
    where s1' = PLOr s1 s2
          s2' = PLOr s2 s1

propAndOrEvaluate :: PLSentence Int -> PLSentence Int -> Bool
propAndOrEvaluate s1 s2 = evaluate valuation s1' == evaluate valuation s2'
    where s1' = PLNot $ PLAnd s1 s2
          s2' = PLOr (PLNot s1) (PLNot s2)

propImpliesEvaluate :: PLSentence Int -> PLSentence Int -> Bool
propImpliesEvaluate s1 s2 = evaluate valuation s1' == evaluate valuation s2'
    where s1' = PLIf s1 s2
          s2' = PLOr (PLNot s1) s2

propIffEvaluate :: PLSentence Int -> PLSentence Int -> Bool
propIffEvaluate s1 s2 = evaluate valuation s1' == evaluate valuation s2'
    where s1' = PLIff s1 s2
          s2' = PLAnd (PLIf s1 s2) (PLIf s2 s1)


{- Task 4 -}
task4 :: TestTree
task4 = testGroup "evaluate properties" [
      QC.testProperty "(not x) and (not y) can be optimized" propNOR1Optimization
    , QC.testProperty "(not x) implies y can be optimized" propNOR2Optimization
    , QC.testProperty "optimized transformation does not increase size" propOptimizedSize
    ]

propNOR1Optimization :: PLSentence Int -> PLSentence Int -> Bool
propNOR1Optimization s1 s2 = norSize (toNOR s) > norSize (toNOROptimized s)
    where s = PLAnd (PLNot s1) (PLNot s2)

propNOR2Optimization :: PLSentence Int -> PLSentence Int -> Bool
propNOR2Optimization s1 s2 = norSize (toNOR s) > norSize (toNOROptimized s)
    where s = PLIf (PLNot s1) s2

propOptimizedSize :: PLSentence Int -> Bool
propOptimizedSize s = norSize (toNOR s) >= norSize (toNOROptimized s)

