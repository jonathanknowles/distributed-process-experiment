{-| Module      : Process
    Description : Utility functions relating to processes.

This module contains more strongly-typed versions of several functions within the
<https://hackage.haskell.org/package/distributed-process/docs/Control-Distributed-Process.html 'Control.Distributed.Process'>
module of the
<https://hackage.haskell.org/package/distributed-process distributed-process> library.

In particular, this module provides:

== Functions that accept 'TimeDuration' values

Certain functions within the original library take values of type 'Int' to
represent time durations (often, but not always in microseconds).

These have have been replaced with functions that take values of type
'TimeDuration':

* 'delay'
* 'expectTimeout'
* 'receiveChanTimeout'

== Functions that accept 'TimePoint' values

These functions provide a convenient way for the caller to specify that a
particular operation should proceed until a specific point in time:

* 'delayUntil'
* 'expectUntil'
* 'receiveChanUntil'

-}

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
