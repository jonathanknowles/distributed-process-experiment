{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Master where

import Context
import Message
import Process
import Slave
import Time

import Control.Distributed.Process                        ( NodeId
                                                          , Process
                                                          , say
                                                          , send
                                                          , spawn )
import Control.Distributed.Process.Closure                ( mkClosure )
import Control.Distributed.Process.Backend.SimpleLocalnet ( Backend
                                                          , terminateAllSlaves )
import Control.Monad                                      ( forM
                                                          , forM_ )
import Data.List                                          ( iterate )
import Data.Semigroup                                     ( (<>) )

master :: Context -> Backend -> [NodeId] -> Process ()
master c@Context {..} backend = \case

    [ ] -> say "FATAL: need a minimum of TWO slaves, but found NONE."
    [_] -> say "FATAL: need a minimum of TWO slaves, but found only ONE."

    slaveNodes -> do
        say $ "found " <> show (length slaveNodes) <> " slaves"
        forM_ slaveNodes $ say . ("slave: " <>) . show

        -- using the master seed, generate a unique seed for each slave:
        let slaveSeeds = take (length slaveNodes) $
                iterate nextMessageSeed contextSeed

        -- spawn a single process on each slave, each with a unique context:
        slavePids <- forM (slaveNodes `zip` slaveSeeds) $
            \(slaveNode, slaveSeed) -> spawn slaveNode
                ($(mkClosure 'slave) c { contextSeed = slaveSeed })

        -- let every slave know about all the other slaves:
        forM_ slavePids $ \slavePid ->
            send slavePid slavePids

        -- wait until the grace period has ended:
        delayUntil $ addTime contextWaitTimeLimit
            $ durationFromInteger 1 Seconds
        say "master: grace period ended: exiting."
        terminateAllSlaves backend
