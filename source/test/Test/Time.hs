{-# LANGUAGE TemplateHaskell #-}

module Test.Time where

import Data.Time

import Time

import Test.QuickCheck ( Arbitrary
                       , Gen
                       , arbitrary
                       , arbitraryBoundedEnum
                       , choose
                       , forAllProperties )

import qualified Test.QuickCheck as QC

instance Arbitrary TimeDuration where
    arbitrary = TimeDuration <$> arbitrary

instance Arbitrary TimePoint where
    arbitrary = TimePoint <$> arbitraryUTCTime

instance Arbitrary TimeUnit where
    arbitrary = arbitraryBoundedEnum

arbitraryUTCTime = do
        y <- choose (2000, 10000) :: Gen Integer
        m <- choose (   1,    12) :: Gen Int
        d <- choose (   1,    29) :: Gen Int
        t <- choose (   0,  tmax) :: Gen Int
        pure $ UTCTime (fromGregorian y m d) (fromIntegral t)
    where
        tmax = 60 * 60 * 24 - 1

prop_timeDifferenceLaw1 = timeDifferenceLaw1
prop_timeDifferenceLaw2 = timeDifferenceLaw2

prop_durationConversionLaw1 = durationConversionLaw1
prop_durationConversionLaw2 = durationConversionLaw2
prop_durationConversionLaw3 = durationConversionLaw3
prop_durationConversionLaw4 = durationConversionLaw4

return []
main = $forAllProperties $ QC.quickCheckWithResult QC.stdArgs { QC.maxSuccess = 1000 }
