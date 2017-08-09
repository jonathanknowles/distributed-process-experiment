{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave where

import Context
import Message
import Process
import Time

import Control.Distributed.Process         ( Process
                                           , SendPort
                                           , ReceivePort
                                           , RemoteTable
                                           , expect
                                           , liftIO
                                           , newChan
                                           , say
                                           , send
                                           , sendChan )
import Control.Distributed.Process.Closure ( remotable )
import Control.Distributed.Process.Node    ( initRemoteTable )
import Control.Monad                       ( forM
                                           , forM_
                                           , replicateM )
import Data.Maybe                          ( catMaybes )
import Data.Semigroup                      ( (<>) )

slave :: Context -> Process ()
slave Context {..} = do

        say $ "starting slave with seed: " <> show contextSeed

        say "receiving process ids of other slaves"
        slavePids <- expect

        say "exchanging ports with other slaves"
        rps <- forM slavePids $ \slavePid -> do
            (sp, rp) <- newChan
            send slavePid sp
            pure rp
        sps <- replicateM (length slavePids) expect

        say "starting slave"
        start sps rps

    where

        start :: [   SendPort MessageBlock]
              -> [ReceivePort MessageBlock]
              -> Process ()
        start sps rps =
            loop contextSeed createMessageDigest where

            loop :: MessageSeed
                 -> MessageDigest
                 -> Process ()
            loop !s !d = do
                t <- liftIO getCurrentTime
                if t < contextSendTimeLimit then do
                    say $ "current score: " <> show d
                    let b = createMessageBlock s contextBlockSize t
                    broadcastBlock b
                    receiveBlocks >>= maybe
                        (finalize d)
                        (loop (blockSeedNext b) . digestMessageBlocks d)
                else finalize d

            -- Sends the given block to every single slave.
            broadcastBlock :: MessageBlock -> Process ()
            broadcastBlock = forM_ sps . flip sendChan

            -- Receives a block from every slave, returning
            -- either just a list of blocks or nothing if at
            -- least one slave failed to send a block.
            receiveBlocks :: Process (Maybe [MessageBlock])
            receiveBlocks = do
                blocks <- catMaybes <$> forM rps
                    (receiveChanUntil contextWaitTimeLimit)
                pure $ if length rps == length blocks
                    then Just blocks
                    else Nothing

            finalize :: MessageDigest -> Process ()
            finalize d = say $ "final score: " <> show d

remotable ['slave]

myRemoteTable :: RemoteTable
myRemoteTable = Slave.__remoteTable initRemoteTable
