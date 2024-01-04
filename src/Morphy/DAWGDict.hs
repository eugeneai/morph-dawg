{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module Morphy.DAWGDict
  ( Dictionary
  , newDictionary
  , freeDictionary
  )
  where

import Foreign.Ptr (Ptr)

data DictionaryClass

type DictionaryPtr = Ptr DictionaryClass

newtype Dictionary = Dictionary {unDict :: DictionaryPtr}

-- #include <dawgdic/completer.h>
-- #include <dawgdic/dictionary.h>
-- #include <hsdawgdic.h>
-- #include <dawgdic/ranked-completer.h>

foreign import ccall unsafe "hsdawgdic.h newDictionary" _newDictionary :: IO DictionaryPtr
foreign import ccall unsafe "hsdawgdic.h freeDictionary" _freeDictionary :: DictionaryPtr -> IO ()

newDictionary :: IO Dictionary
newDictionary = do
  dict <- _newDictionary
  return . Dictionary $ dict

freeDictionary :: Dictionary -> IO ()
freeDictionary dict = do
  _freeDictionary $ unDict dict
  return ()
