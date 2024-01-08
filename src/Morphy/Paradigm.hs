module Morphy.Paradigm
  (
    Paradigms
  , Para
  , fromFile
  ) where

import qualified Data.Vector as V
import Data.Word (Word16)
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import System.IO.Unsafe (unsafePerformIO)


type Para = V.Vector Word16
type Paradigms = V.Vector Para

fromFile :: String -> Paradigms
fromFile fn = unsafePerformIO $ gen fn
  where
    gen fn = do
      h <- openFile fn ReadMode
      ctbs <- BS.hGetSome h 2
      let gslen = G.runGet getWord16 $ BL.fromStrict ctbs
      let rc = V.generateM (fromIntegral gslen) $ pgens h
--      p <- hTell h
--      s <- hFileSize h
--      putStr "\npos:"
--      putStrLn . show $ p
--      putStr "size:"
--      putStrLn . show $ s
      rc

    getWord16 :: G.Get Word16
    getWord16 = do
      G.getWord16le

    pgens h i = do
      bs <- BS.hGetSome h 2
      let plen = G.runGet getWord16 $ BL.fromStrict bs
      -- print $ show i ++ "-th LEN:" ++ show plen
      V.generateM (fromIntegral plen) (genp h)

    genp h _ = do
      bs <- BS.hGetSome h 2
      return $ G.runGet getWord16 $ BL.fromStrict bs
