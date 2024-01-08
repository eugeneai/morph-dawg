module Main (main) where

import Morphy ( Parse, morphParse )
import Morphy.DAWG
  (
    fromDir
  , follow
  , lookupData
  , putTuplesLn
  )

import qualified Data.Text as T
import Main.Utf8 (withUtf8)
import Data.ByteString.Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as C8



-- Test like function

testVal :: [Parse]
testVal = morphParse $ T.pack "стали"


main :: IO ()
main = withUtf8 $ do
  -- putStrLn $ show dawg
  putTuplesLn idxs
  putStrLn ""
  putStrLn $ show testVal
  where
    f word =
      let idx = lookupData dawg word cnv
      in idx
    dawg = fromDir "corpora/ru"
    idxs = concat $ map f ["стали", "стали", "стали", "сталь", "встали", "мама", "машина"]
    cnv x = decodeBase64Lenient x
