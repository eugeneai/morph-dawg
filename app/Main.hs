module Main (main) where

import Lib ( someFunc, word, Parse )
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn $ show someFunc
