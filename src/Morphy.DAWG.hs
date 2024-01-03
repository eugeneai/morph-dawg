{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Morphy.DAWG
    ( someFunc
    , morphParse
    , word
    , Parse
    ) where

import qualified Data.Text as T
import Prelude.Compat
    ( (++),
      otherwise,
      map,
      ($),
      Eq((==)),
      Num((*)),
      Ord((<=), compare),
      Read,
      Show(show),
      Bool(..),
      String,
      Float,
      Int,
      Maybe(..),
      Foldable,
      all,
      maybe,
      (.),
      (+),
      (&&),
      (/=),
      (||) )
import Data.String
import Data.Set as S

data Tag = NOUN | INAN | FEMN | SING | DATV -- |
  deriving (Show, Eq, Ord)

data Parse = Parse
  {
    word :: T.Text
  , tag :: S.Set Tag
  , normalForm :: T.Text
  , score :: Float
  -- , methodStack ::
  } deriving (Eq)

instance Show Parse where
  show p = "{" ++ ((T.unpack . word) p) ++ ":" ++ ((T.unpack . normalForm) p) ++
    ((show . S.toList . tag) p) ++ ((show . score) p) ++ "}"

morphParse :: T.Text -> [Parse]
morphParse word = [
  Parse {word=word, tag=S.fromList [NOUN], normalForm=word, score=1.0}
  , Parse {word=word, tag=S.fromList [NOUN], normalForm=word, score=0.5}
  ]

-- Test like function

someFunc :: [Parse]
someFunc = morphParse $ T.pack "стали"
