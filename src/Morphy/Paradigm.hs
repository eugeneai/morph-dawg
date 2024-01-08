module Morphy.Paradigm
  (
    Paradigms
  , Para
  , fromFile
  ) where

import qualified Data.Vector as V
import Data.Word (Word16)
import System.IO
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import System.IO.Unsafe (unsafePerformIO)


type Para = V.Vector Word16
type Paradigms = V.Vector Para

fromFile :: String -> Paradigms
fromFile fn = unsafePerformIO $ gen fn
  where
    gen fn = do
      cts <- BL.readFile $ fn
      let ct = cts
      let pslen = G.runGet getWord16 ct
      print pslen
      V.generateM (fromIntegral pslen) $ pgens ct

    getWord16 :: G.Get Word16
    getWord16 = do
      G.getWord16le

    pgens ct _ = do
      let plen = G.runGet getWord16 ct
      V.generateM (fromIntegral plen) (genp ct)

    genp ct _ = do
      return $ G.runGet getWord16 ct
