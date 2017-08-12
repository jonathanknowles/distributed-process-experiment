{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Slave where

import Context
import Message
import Process
import Time

import Control.Concurrent.MVar             ( MVar
                                           , newMVar
                                           , readMVar
                                           , swapMVar )
import Control.Distributed.Process         ( Process
                                           , SendPort
                                           , ReceivePort
                                           , RemoteTable
                                           , expect
                                           , liftIO
                                           , newChan
                                           , say
                                           , send
                                           , sendChan
                                           , spawnLocal )
import Control.Distributed.Process.Closure ( remotable )
import Control.Distributed.Process.Node    ( initRemoteTable )
import Control.Monad                       ( forever
                                           , forM
                                           , forM_
                                           , replicateM
                                           , void
                                           , when )
import Data.Maybe                          ( catMaybes )
import Data.Semigroup                      ( (<>) )

slave :: Context -> Process ()
slave c@Context {..} = do

    say $ "starting slave with seed: " <> show contextSeed
    say "receiving process ids of other slaves"
    slavePids <- expect
    say "exchanging ports with other slaves"
    rps <- forM slavePids $ \slavePid -> do
        (sp, rp) <- newChan
        send slavePid sp
        pure rp
    sps <- replicateM (length slavePids) expect
    md <- liftIO $ newMVar createMessageDigest
    say "starting slave"
    start c md sps rps

start
    :: Context
    -> MVar MessageDigest
    -> [   SendPort MessageBlock]
    -> [ReceivePort MessageBlock]
    -> Process ()
start Context {..} md sps rps = do

    void $ spawnLocal progressReportLoop
    sendReceiveLoop contextSeed createMessageDigest

    where

        progressReportLoop :: Process ()
        progressReportLoop = forever $ do
            cp <- liftIO $ readMVar md
            say $ "current progress: " <> show cp
            delay $ durationFromInteger 1 Seconds

        sendReceiveLoop :: MessageSeed -> MessageDigest -> Process ()
        sendReceiveLoop !s !d = do
            _ <- liftIO $ swapMVar md d
            t <- liftIO getCurrentTime
            when (t < contextSendTimeLimit) $ do
                let b = createMessageBlock s contextBlockSize t
                broadcastBlock b
                fetchBlocks >>= maybe (pure ())
                    (sendReceiveLoop
                        (blockSeedNext b) . digestMessageBlocks d)

        -- Sends the given block to every single slave.
        broadcastBlock :: MessageBlock -> Process ()
        broadcastBlock = forM_ sps . flip sendChan

        -- Receives a block from every slave, returning
        -- either just a list of blocks or nothing if at
        -- least one slave failed to send a block.
        fetchBlocks :: Process (Maybe [MessageBlock])
        fetchBlocks = do
            blocks <- catMaybes <$> forM rps
                (receiveChanUntil contextWaitTimeLimit)
            pure $ if length rps == length blocks
                then Just blocks
                else Nothing

remotable ['slave]

myRemoteTable :: RemoteTable
myRemoteTable = Slave.__remoteTable initRemoteTable
