module Main (main) where

import Morphy.DAWG ( someFunc, word, Parse )
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn $ show someFunc
