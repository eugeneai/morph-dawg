{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Morphy.Grammeme
  (
    GramTag(..)
  , Grammeme
  , Grammemes
  , fromFile
  ) where

import qualified Data.Vector as V
import Data.Word (Word16)
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import System.IO.Unsafe (unsafePerformIO)
import Prelude.Compat
import Data.Aeson
import Control.Applicative (empty)
import Data.List
import qualified Data.Text as T
import qualified Text.Read.Compat as TR
import Data.List.Extra


data GramTag = BAD | NONE | U String
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
  deriving (Show, Read, Eq, Ord)


data Grammeme = Gram
  {
    pos    :: !GramTag
  , parent :: !GramTag
  , posr   :: !String
  , descr  :: !String
  }
  deriving (Show, Eq)

type Grammemes = V.Vector Grammeme

fromFile :: String -> Grammemes
fromFile fn = unsafePerformIO $ gen
  where
    gen :: IO (Grammemes)
    gen = do
      ct <- BL.readFile $ fn
      let dec = decode ct :: Maybe Value
      -- print dec
      case dec of
        Just arr -> do
          case arr of
            Array lst -> do
              let gls = V.map f lst
              print ">>>"
              -- print . V.filter bd $ gls
              print . V.length $ gls
              return gls
            _ -> return V.empty
        _ -> return V.empty
    f :: Value -> Grammeme
    f (Array vec) =
      let ls = V.toList vec
      in
        case map ff ls of
          [poses, parents, posr, descr] ->
            Gram {pos = rp poses,
                  parent = rp parents,
                  posr=posr, descr=descr}
          _ -> Gram {pos = BAD, parent = NONE, posr="ttt", descr="ddd"}
    ff :: Value -> String
    ff (String s) = upper . T.unpack $ s
    rp = readGram
    bd :: Grammeme -> Bool
    bd v = case pos v of
             BAD -> True
             _ -> False

readGram :: String -> GramTag
readGram t =
  let
    rpl = upper . replace "-" "_" $ t
    rm = TR.readMaybe rpl :: Maybe GramTag
  in
    case rm of
      Nothing -> case rpl of
        "1PER" -> PER1
        "2PER" -> PER3
        "3PER" -> PER3
        "" -> NONE
        _ -> U rpl
      Just g -> g
