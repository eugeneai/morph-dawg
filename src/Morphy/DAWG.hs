{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Morphy.DAWG
  (
    DAWG
  , fromFile
  , follow
  , lookupData
  , putTuplesLn
  -- , freeDawg
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
      (||),
      IO,
      putStr,
      putStrLn,
      fst,
      snd,
      return)
import Data.String
import qualified Data.Set as S
import Morphy.DAWGDict (
  Dictionary
  , newDictionary
  , readDictionaryFromFile
  , followDictionary
  , valueDictionary
  )

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.UTF8 as BLU


data Tag = BAD
  | POST  | NOUN  | ADJF  | ADJS  | COMP  | VERB  | INFN  | PRTF  | PRTS
  | GRND  | NUMR  | ADVB  | NPRO  | PRED  | PREP  | CONJ  | PRCL  | INTJ  | ANIM
  | INAN  | GNDR  | MASC  | FEMN  | NEUT  | MS_F  | NMBR  | SING  | PLUR  | SGTM
  | PLTM  | FIXD  | CASE  | NOMN  | GENT  | DATV  | ACCS  | ABLT  | LOCT  | VOCT
  | GEN1  | GEN2  | ACC2  | LOC1  | LOC2  | ABBR  | NAME  | SURN  | PATR  | GEOX
  | ORGN  | TRAD  | SUBX  | SUPR  | QUAL  | APRO  | ANUM  | POSS  | V_EY  | V_OY
  | CMP2  | V_EJ  | ASPC  | PERF  | IMPF  | TRNS  | TRAN  | INTR  | IMPE  | IMPX
  | MULT  | REFL  | PERS  | PER1  | PER2  | PER3  | TENS  | PRES  | PAST  | FUTR
  | MOOD  | INDC  | IMPR  | INVL  | INCL  | EXCL  | VOIC  | ACTV  | PSSV  | INFR
  | SLNG  | ARCH  | LITR  | ERRO  | DIST  | QUES  | DMNS  | PRNT  | V_BE  | V_EN
  | V_IE  | V_BI  | FIMP  | PRDX  | COUN  | COLL  | V_SH  | AF_P  | INMX  | VPRE
  | ANPH  | INIT  | ADJX  | HYPO  | LATN  | UNKN
  deriving (Show, Eq, Ord)



data Parse = Parse
  {
    word :: T.Text
  , tag :: S.Set Tag
  , normalForm :: T.Text
  , score :: Float
  -- , methodStack ::
  } deriving (Eq)

data IntTuple a = IntTuple {key::String, val::Maybe a} deriving Eq

instance (Show a) => Show (IntTuple a) where
  show t = "{" ++ key t ++ "=>" ++ (show . val $ t) ++ "}"

instance Show Parse where
  show p = "{" ++ (T.unpack . word $ p) ++ ":" ++ (T.unpack . normalForm $ p) ++
    (show . S.toList . tag $ p) ++ (show . score $ p) ++ "}"

morphParse :: T.Text -> [Parse]
morphParse word = [
  Parse {word=word, tag=S.fromList [NOUN], normalForm=word, score=1.0}
  , Parse {word=word, tag=S.fromList [NOUN], normalForm=word, score=0.5}
  ]

newtype DAWG = DAWG { dict::Dictionary }
  deriving Show

fromFile :: String -> DAWG
fromFile fn = unsafePerformIO $ createAndOpen fn
  where
    createAndOpen :: String -> IO DAWG
    createAndOpen fn = do
      dict <- newDictionary
      readDictionaryFromFile dict fn
      return . DAWG $ dict

follow :: DAWG -> String -> Int -> [(String, Maybe Int)]
follow dawg str index =
  let d = dict dawg
  in
    [(str, followDictionary d str index)]

lookupData :: DAWG -> String -> [(String, Maybe BLU.ByteString)]
lookupData dawg str =
  let ls = follow dawg str 0
  in
    map go ls
  where
    go (word, index) =
      let
        d = dict dawg
      in
        case index of
          Nothing -> (word, Nothing)
          Just idx -> (word, valueDictionary d idx)

putTuplesLn :: (Show a) => [(String, Maybe a)] -> IO ()
putTuplesLn xs = do
  putStr "["
  putTuples' xs
  putStr "]\n"
  where
    putTuples' [] = return ()
    putTuples' [x] = do
      pt x
    putTuples' (x:y:xs) = do
      pt x
      putStr ", "
      putTuples' (y:xs)
    pt x = do
      putStr . fst $ x
      putStr "=>"
      putStr . show . snd $ x



-- freeDawg :: DAWG -> IO ()
-- freeDawg dawg = do
--   freeDictionary . dict $ dawg
--   return ()

-- Test like function

someFunc :: [Parse]
someFunc = morphParse $ T.pack "стали"
