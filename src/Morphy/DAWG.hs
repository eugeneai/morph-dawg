{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Morphy.DAWG
  (
    DAWG
  , fromDir
  , follow
  , lookupData
  , lookupParadigms
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
      fromIntegral,
      return)
import Data.String
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Morphy.DAWGDict (
  Dictionary
  , newDictionary
  , readDictionaryFromFile
  , followDictionary
  , valueDictionary
  )

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString as BL
import qualified Morphy.Paradigm as P
import qualified Data.Binary.Get as G
import Data.ByteString.Base64


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

instance (Show a) => Show (IntTuple a)  where
  show t = "{" ++ key t ++ "=>" ++ (show . val $ t) ++ "}"

instance Show Parse where
  show p = "{" ++ (T.unpack . word $ p) ++ ":" ++ (T.unpack . normalForm $ p) ++
    (show . S.toList . tag $ p) ++ (show . score $ p) ++ "}"

morphParse :: T.Text -> [Parse]
morphParse word = [
  Parse {word=word, tag=S.fromList [NOUN], normalForm=word, score=1.0}
  , Parse {word=word, tag=S.fromList [NOUN], normalForm=word, score=0.5}
  ]

data DAWG = DAWG {
    dict  :: !Dictionary
  , paras :: !P.Paradigms
  }
  deriving Show

fromDir :: String -> DAWG
fromDir dirName =
  let
    dict = dictFromFile (dirName ++ "/" ++ "words.dawg")
    paras = P.fromFile (dirName ++ "/" ++ "paradigms.array")
  in
    DAWG {dict=dict, paras=paras}


dictFromFile :: String -> Dictionary
dictFromFile fn = unsafePerformIO $ createAndOpen fn
  where
    createAndOpen :: String -> IO Dictionary
    createAndOpen fn = do
      dict <- newDictionary
      readDictionaryFromFile dict fn
      return dict

follow :: DAWG -> String -> Int -> [(String, Maybe Int)]
follow dawg str index =
  let d = dict dawg
  in
    [(str, followDictionary d str index)]

lookupData :: DAWG -> String -> (BS.ByteString -> a) -> [(String, [(String, a)])]
lookupData dawg str f =
  let ls = follow dawg str 0
  in
    map go ls
  where
    go (word, index) =
      let
        d = dict dawg
      in
        case index of
          Nothing -> (word, [])
          Just idx -> (word, valueDictionary d idx f)

lookupParadigms' :: DAWG -> String -> [(String, [(String, (P.Para, Int))])]
lookupParadigms' dawg key =
  let
    idxs = lookupData dawg key f
  in
    idxs
  where
    f bs =
      let
        dsl = BL.fromStrict . decodeBase64Lenient $ bs
        b1 = BLU.take 2 $ dsl
        b2 = BLU.drop 2 $ dsl
        (pn, idx) = (fi b1, fi b2)
        pg = paras dawg V.! pn
      in
        (pg, idx)

    fi b = fromIntegral $ G.runGet getWord16 b
    getWord16 = do G.getWord16be

lookupParadigms = lookupParadigms'

putTuplesLn :: (Show a) => [(String, [(String, a)])] -> IO ()
putTuplesLn xs = do
  putStr " ["
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
      putStr "=>["
      ps . snd $ x
      putStr "]"
    ps [] = return ()
    ps ((word, idx):xs) = do
      putStr word
      putStr "->"
      putStr . show $ idx
      -- putStr . show $ idx
      putStr ", "
      ps xs




-- freeDawg :: DAWG -> IO ()
-- freeDawg dawg = do
--   freeDictionary . dict $ dawg
--   return ()

-- Test like function

someFunc :: [Parse]
someFunc = morphParse $ T.pack "стали"
