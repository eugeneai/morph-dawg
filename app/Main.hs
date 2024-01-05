module Main (main) where

import Morphy ( Parse, morphParse )
import Morphy.DAWG ( fromFile )
import qualified Data.Text as T

-- Test like function

testVal :: [Parse]
testVal = morphParse $ T.pack " стали"


main :: IO ()
main = do
  let dawg = fromFile "/tmp/nofile"
  putStrLn $ show dawg
  putStrLn $ show testVal
