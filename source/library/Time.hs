{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Time
    ( Rounding     (..)
    , TimeDuration (..)
    , TimePoint    (..)
    , TimeUnit     (..)
    -- basic operations
    , addTime
    , timeDifference
    , getCurrentTime
    -- conversions
    , durationFromInteger
    , durationFromIntegral
    , durationToIntegral
    , durationToInteger
    , durationToNominalDiffTime
    , durationFromNominalDiffTime
    -- laws
    , timeDifferenceLaw1
    , timeDifferenceLaw2
    , durationConversionLaw1
    , durationConversionLaw2
    , durationConversionLaw3
    , durationConversionLaw4
    ) where

import Data.Binary                 ( Binary
                                   , get
                                   , put )
import Data.Binary.Get             ( Get )
import Data.Binary.Put             ( Put )
import Data.Ratio                  ( denominator
                                   , numerator )
import Data.Time                   ( Day (..)
                                   , DiffTime
                                   , NominalDiffTime
                                   , UTCTime (..)
                                   , addUTCTime
                                   , diffUTCTime )
import GHC.Generics                ( Generic )

import qualified Data.Time as T

-- internally, an integer number of picoseconds.
newtype TimeDuration = TimeDuration Integer
    deriving (Eq, Generic, Ord, Show)

newtype TimePoint = TimePoint UTCTime
    deriving (Eq, Generic, Ord, Show)

data TimeUnit
    = Picos
    | Nanos
    | Micros
    | Millis
    | Seconds
    | Minutes
    | Hours
    deriving (Bounded, Enum, Show)

data Rounding
    = RoundingUp
    | RoundingDown

unitSize :: TimeUnit -> Integer
unitSize = \case
    Picos   -> 1
    Nanos   -> 1000
    Micros  -> 1000 * 1000
    Millis  -> 1000 * 1000 * 1000
    Seconds -> 1000 * 1000 * 1000 * 1000
    Minutes -> 1000 * 1000 * 1000 * 1000 * 60
    Hours   -> 1000 * 1000 * 1000 * 1000 * 60 * 60

durationFromIntegral :: Integral a => a -> TimeUnit -> TimeDuration
durationFromIntegral a u = TimeDuration $ fromIntegral a * unitSize u

durationFromInteger :: Integer -> TimeUnit -> TimeDuration
durationFromInteger = durationFromIntegral

durationToIntegral :: Integral a => TimeDuration -> TimeUnit -> Rounding -> a
durationToIntegral (TimeDuration d) u r =
        fromIntegral $ n `div` unitSize u
    where
        n = case r of
            RoundingDown -> d
            RoundingUp   -> d + unitSize u - 1

durationToInteger :: TimeDuration -> TimeUnit -> Rounding -> Integer
durationToInteger = durationToIntegral

durationToNominalDiffTime :: TimeDuration -> NominalDiffTime
durationToNominalDiffTime (TimeDuration d) =
    fromIntegral d / fromIntegral (unitSize Seconds)

durationFromNominalDiffTime :: NominalDiffTime -> TimeDuration
durationFromNominalDiffTime t =
        TimeDuration $ n * unitSize Seconds `div` d
    where
        r = toRational t
        n = numerator r
        d = denominator r

getCurrentTime :: IO TimePoint
getCurrentTime = TimePoint <$> T.getCurrentTime

class AddTime a b c | a b -> c where
    addTime :: a -> b -> c

instance AddTime TimePoint TimeDuration TimePoint where
    addTime (TimePoint a) d =
        TimePoint (durationToNominalDiffTime d `addUTCTime` a)

instance AddTime TimeDuration TimePoint TimePoint where
    addTime d (TimePoint a) =
        TimePoint (durationToNominalDiffTime d `addUTCTime` a)

instance AddTime TimeDuration TimeDuration TimeDuration where
    addTime (TimeDuration d1) (TimeDuration d2) =
        TimeDuration $ d1 + d2

-- timeDifference a b calculates the duration of time that elapses
-- from time point a to time point b.
timeDifference :: TimePoint -> TimePoint -> TimeDuration
timeDifference (TimePoint a) (TimePoint b) =
    durationFromNominalDiffTime $ diffUTCTime b a

-- Instances

instance Binary TimePoint where
    put (TimePoint t) = putUtcTime t
    get = TimePoint <$> getUtcTime

putUtcTime :: UTCTime -> Put
putUtcTime (UTCTime a b) = putDay a >> putDiffTime b
getUtcTime :: Get UTCTime
getUtcTime = UTCTime <$> getDay <*> getDiffTime

putDay :: Day -> Put
putDay (ModifiedJulianDay d) = put d
getDay :: Get Day
getDay = ModifiedJulianDay <$> get

putDiffTime :: DiffTime -> Put
putDiffTime = put . fromEnum
getDiffTime :: Get DiffTime
getDiffTime = toEnum <$> get

-- Laws

timeDifferenceLaw1 :: TimePoint -> TimePoint -> Bool
timeDifferenceLaw1 a b =
    addTime a (timeDifference a b) == b

timeDifferenceLaw2 :: TimePoint -> TimeDuration -> Bool
timeDifferenceLaw2 a b =
    timeDifference a (addTime a b) == b

durationConversionLaw1 :: Integer -> TimeUnit -> Bool
durationConversionLaw1 i u =
    durationToInteger (durationFromInteger i u) u RoundingUp == i

durationConversionLaw2 :: Integer -> TimeUnit -> Bool
durationConversionLaw2 i u =
    durationToInteger (durationFromInteger i u) u RoundingDown == i

durationConversionLaw3 :: TimeDuration -> TimeUnit -> Bool
durationConversionLaw3 d u =
    durationFromInteger (durationToInteger d u RoundingUp) u >= d

durationConversionLaw4 :: TimeDuration -> TimeUnit -> Bool
durationConversionLaw4 d u =
    durationFromInteger (durationToInteger d u RoundingDown) u <= d
