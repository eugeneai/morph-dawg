module Main (main) where

import Morphy ( word, Parse, morphParse )
import qualified Data.Text as T

-- Test like function

testVal :: [Parse]
testVal = morphParse $ T.pack "стали"


main :: IO ()
main = do
  putStrLn $ show testVal
