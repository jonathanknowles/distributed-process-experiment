{-| Module      : Pretty
    Description : Pretty-printing functions

Various pretty printing functions.
-}

{-# LANGUAGE OverloadedStrings #-}

module Pretty
    ( prettyInteger
    , prettyIntegerT
    ) where

import Data.Monoid ( (<>) )
import Data.Text   ( Text )

import qualified Data.Text as T

prettyInteger :: (Num a, Ord a, Show a) => a -> String
prettyInteger = T.unpack . prettyIntegerT

prettyIntegerT :: (Num a, Ord a, Show a) => a -> Text
prettyIntegerT a =
    if a < 0
    then "-" <> prettyIntegerT (-a)
    else T.reverse
       $ T.intercalate ","
       $ T.chunksOf 3
       $ T.reverse
       $ T.pack
       $ show a
