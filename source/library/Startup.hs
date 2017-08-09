{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Startup where

import CLI
import Config
import Context
import Slave
import Master
import Time

import Control.Distributed.Process.Backend.SimpleLocalnet ( initializeBackend
                                                          , startMaster
                                                          , startSlave )

import qualified System.IO as S

main :: IO ()
main = do
    S.hSetBuffering S.stdout S.NoBuffering
    run =<< getConfig

run :: Config -> IO ()
run = \case
    Slave c@SlaveConfig {..} -> do
        backend <- initializeBackend
            (getHost c) (getPort c) myRemoteTable
        startSlave backend
    Master c@MasterConfig {..} -> do
        backend <- initializeBackend
            (getHost c) (getPort c) myRemoteTable
        context <- contextFromConfig masterPoolConfig
        startMaster backend (master context backend)

contextFromConfig :: PoolConfig -> IO Context
contextFromConfig PoolConfig {..} =
        convert <$> getCurrentTime
    where
        convert :: TimePoint -> Context
        convert currentTime = Context {..}
            where
                contextSeed = configSeed
                contextBlockSize = configBlockSize
                contextSendTimeLimit = addTime currentTime configSendPeriod
                contextWaitTimeLimit = addTime contextSendTimeLimit configWaitPeriod
