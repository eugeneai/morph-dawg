module Main (main) where

import Morphy ( word, Parse, morphParse )
import Morphy.DAWG ( DAWG, fromFile )
import qualified Data.Text as T

-- Test like function

testVal :: [Parse]
testVal = morphParse $ T.pack "стали"


main :: IO ()
main = do
  let dawg = fromFile ""
  -- freeDawg dawg
  putStrLn $ show testVal
