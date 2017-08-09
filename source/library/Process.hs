module Process
    ( delay
    , delayUntil
    , expectTimeout
    , expectUntil
    , receiveChanTimeout
    , receiveChanUntil
    ) where

import Control.Concurrent                       ( threadDelay )
import Control.Distributed.Process              ( Process
                                                , ReceivePort
                                                , liftIO )
import Control.Distributed.Process.Serializable ( Serializable )

import Time

import qualified Control.Distributed.Process as P

delay :: TimeDuration -> Process ()
delay d = liftIO $ threadDelay $ durationToMicros d

delayUntil :: TimePoint -> Process ()
delayUntil p = do
    t <- liftIO getCurrentTime
    delay (timeDifference t p)

expectTimeout :: Serializable a => TimeDuration -> Process (Maybe a)
expectTimeout = P.expectTimeout . durationToMicros

expectUntil ::  Serializable a => TimePoint -> Process (Maybe a)
expectUntil p = do
    t <- liftIO getCurrentTime
    expectTimeout (timeDifference t p)

receiveChanTimeout :: Serializable a => TimeDuration -> ReceivePort a -> Process (Maybe a)
receiveChanTimeout = P.receiveChanTimeout . durationToMicros

receiveChanUntil :: Serializable a => TimePoint -> ReceivePort a -> Process (Maybe a)
receiveChanUntil p r = do
    t <- liftIO getCurrentTime
    receiveChanTimeout (timeDifference t p) r

durationToMicros :: TimeDuration -> Int
durationToMicros d = durationToIntegral d Micros RoundingUp
