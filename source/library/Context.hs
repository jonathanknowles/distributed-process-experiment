{-| Module      : Context
    Description : A run-time context for all nodes.

This module defines a 'Context' type, passed to all nodes during startup.

The master receives a 'Context' object during startup, and then passes a copy of
this 'Context' (with a unique seed) to each slave.
-}

{-# LANGUAGE DeriveGeneric #-}

module Context where

import Message
import Time

import Data.Binary  ( Binary )
import GHC.Generics ( Generic )

data Context = Context
    { contextBlockSize     :: MessageCount
    , contextSeed          :: MessageSeed
    , contextSendTimeLimit :: TimePoint
    , contextWaitTimeLimit :: TimePoint }
    deriving Generic

instance Binary Context
