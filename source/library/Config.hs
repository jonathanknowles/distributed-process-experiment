module Config
    ( Config       (..)
    , MasterConfig (..)
    , SlaveConfig  (..)
    , BindConfig   (..)
    , PoolConfig   (..)
    , getPort
    , getHost
    ) where

import Message
import Time

data Config = Master MasterConfig | Slave SlaveConfig
    deriving Show

data MasterConfig = MasterConfig
    { masterBindConfig :: BindConfig
    , masterPoolConfig :: PoolConfig }
    deriving Show

data SlaveConfig = SlaveConfig
    { slaveBindConfig :: BindConfig }
    deriving Show

data BindConfig = BindConfig
    { configHost :: String
    , configPort :: String }
    deriving Show

data PoolConfig = PoolConfig
    { configSendPeriod :: TimeDuration
    , configWaitPeriod :: TimeDuration
    , configSeed       :: MessageSeed
    , configBlockSize  :: MessageCount }
    deriving Show

class HasPort a where getPort :: a -> String
class HasHost a where getHost :: a -> String

instance HasPort MasterConfig where
    getPort = configPort . masterBindConfig
instance HasPort SlaveConfig where
    getPort = configPort . slaveBindConfig
instance HasHost MasterConfig where
    getHost = configHost . masterBindConfig
instance HasHost SlaveConfig where
    getHost = configHost . slaveBindConfig
