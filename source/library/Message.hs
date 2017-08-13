{-| Module      : Message
    Description : Message generation and consumption

This module defines the 'Message', 'MessageBlock', 'MessageDigest' and
'MessageSeed' types, as well as functions for generating and digesting
messages.

== Creating messages

To create messages, first create an initial 'MessageSeed' with the
'createMessageSeed' function.

With a 'MessageSeed' in hand, use either the 'createMessage' function or the
'createMessageBlock' function to create messages.

Each of these functions returns a new 'MessageSeed' suitable for use in
subsequent calls.

== Digesting messages

To digest messages, first create an initial 'MessageDigest' with the
'createMessageDigest' function.

With a 'MessageDigest' in hand, use either the 'digestMessage' function, the
'digestMessageBlock' function, or the 'digestMessageBlocks' function to digest
messages.

Each of these functions returns a new 'MessageDigest' suitable for use in
subsequent calls.

-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Message
    ( Message
    , MessageBlock  (..)
    , MessageCount  (..)
    , MessageDigest (..)
    , MessageSeed   (..)
    , createMessage
    , createMessageBlock
    , createMessageSeed
    , createMessageDigest
    , digestMessage
    , digestMessageBlock
    , digestMessageBlocks
    , nextMessageSeed
    ) where

import Control.Monad.ST             ( ST
                                    , runST )
import Data.Binary                  ( Binary
                                    , get
                                    , put )
import Data.List                    ( sort )
import Data.Monoid                  ( (<>) )
import Data.Vector.Binary           ( )
import Data.Vector.Unboxed          ( Vector )
import Data.Vector.Unboxed.Deriving ( derivingUnbox )
import GHC.Generics                 ( Generic )
import Pretty                       ( prettyInteger )
import System.Random                ( mkStdGen
                                    , next
                                    , randomR
                                    , StdGen )
import Time                         ( TimePoint )

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U

newtype Message = Message Double
    deriving (Generic, Num, Show)

data MessageBlock = MessageBlock
    { blockContents :: !(Vector Message)
    , blockSize     :: !MessageCount
    , blockSeed     :: !MessageSeed
    , blockSeedNext :: !MessageSeed
    , blockTime     :: !TimePoint }
    deriving (Generic)

newtype MessageCount = MessageCount Int
    deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

data MessageDigest = MessageDigest
    { digestSize :: {-# UNPACK #-} !MessageCount
    , digestSum  :: {-# UNPACK #-} !Message }

newtype MessageSeed = MessageSeed StdGen
    deriving (Generic)

instance Binary Message
instance Binary MessageBlock
instance Binary MessageCount

instance Binary MessageSeed where
    put (MessageSeed s) = put $ show s
    get = MessageSeed . read <$> get

instance Eq MessageBlock where
    a == b = blockSeed a == blockSeed b
          && blockSize a == blockSize b

instance Eq MessageSeed where
    (MessageSeed a) == (MessageSeed b) =
        show a == show b

instance Ord MessageBlock where
    b1 `compare` b2 = key b1 `compare` key b2
        where key b = (blockTime b, blockSeed b, blockSize b)

instance Ord MessageSeed where
    compare (MessageSeed a) (MessageSeed b) =
        compare (show a) (show b)

instance Show MessageBlock where
    show b = "MessageBlock " <> show (blockTime b, blockSeed b, blockSize b)

instance Show MessageDigest where
    show (MessageDigest (MessageCount c) (Message s)) =
        "MessageDigest {size: "
            <> prettyInteger c <> " sum: "
            <> show s <> "}"

instance Show MessageSeed where
    show (MessageSeed s) =
        "MessageSeed {" <> show s <> "}"

derivingUnbox "Message"
    [t|Message -> Double|] [|unboxMessage|] [|boxMessage|]

unboxMessage :: Message -> Double
unboxMessage (Message m) = m

boxMessage :: Double -> Message
boxMessage = Message

createMessage :: MessageSeed -> (Message, MessageSeed)
createMessage (MessageSeed s) = (Message m, MessageSeed t)
    where (m, t) = randomR (0.0, 1.0) s
{-# INLINE createMessage #-}

createMessageBlock
    :: MessageSeed
    -> MessageCount
    -> TimePoint
    -> (MessageBlock, MessageSeed)
createMessageBlock s (MessageCount c) t = runST $ do
        v <- M.unsafeNew c
        u <- initialize v
        b <- U.freeze v
        pure (MessageBlock b (MessageCount c) s u t, u)
    where
        initialize :: forall s. U.MVector s Message -> ST s MessageSeed
        initialize v = loop s 0 where
            loop :: MessageSeed -> Int -> ST s MessageSeed
            loop !w !i
                | i >= c = pure w
                | i  < c = do let (m, u) = createMessage w
                              M.write v i m
                              loop u $ succ i

createMessageSeed :: Int -> MessageSeed
createMessageSeed = MessageSeed . mkStdGen

nextMessageSeed :: MessageSeed -> MessageSeed
nextMessageSeed (MessageSeed s) = MessageSeed $ snd $ next s

createMessageDigest :: MessageDigest
createMessageDigest = MessageDigest 0 (Message 0.0)

digestMessage :: MessageDigest -> Message -> MessageDigest
digestMessage (MessageDigest c d) m =
    MessageDigest c' (d + m * fromIntegral c')
        where c' = succ c

digestMessageBlock :: MessageDigest -> MessageBlock -> MessageDigest
digestMessageBlock d b = U.foldl' digestMessage d (blockContents b)

{-| Digest multiple message blocks in ascending order of their timestamps,
    as given by the 'blockTime' function.
-}
digestMessageBlocks :: MessageDigest -> [MessageBlock] -> MessageDigest
digestMessageBlocks d bs = foldr (flip digestMessageBlock) d (sort bs)
