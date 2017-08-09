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
