module Main (main) where

import Morphy ( Parse, morphParse )
import Morphy.DAWG ( fromFile, follow, putTuplesLn )
import qualified Data.Text as T
import Main.Utf8 (withUtf8)

-- Test like function

testVal :: [Parse]
testVal = morphParse $ T.pack "стали"


main :: IO ()
main = withUtf8 $ do
  putStrLn $ show dawg
  putTuplesLn idxs
  putStrLn ""
  putStrLn $ show testVal
  where
    f word = follow dawg word 0
    dawg = fromFile "corpora/ru/words.dawg"
    idxs = concat $ map f ["стали", "стали", "стали", "сталь", "встали", "мама", "я", "яя", "яяя", "яяяя", "яяяяя"]
