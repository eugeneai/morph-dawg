{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module Morphy.DAWGDict
  ( Dictionary
  , newDictionary
  -- , freeDictionary
  )
  where

import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)

data DictionaryClass

newtype Dictionary = Dictionary {unDict :: ForeignPtr DictionaryClass}

-- #include <dawgdic/completer.h>
-- #include <dawgdic/dictionary.h>
-- #include <hsdawgdic.h>
-- #include <dawgdic/ranked-completer.h>

foreign import ccall unsafe "hsdawgdic.h newDictionary"
   _newDictionary :: IO (Ptr DictionaryClass)
foreign import ccall unsafe "hsdawgdic.h &freeDictionary"
   _freeDictionary :: FunPtr(Ptr DictionaryClass -> IO ())

newDictionary :: IO Dictionary
newDictionary = do
  dict <- _newDictionary
  if dict == nullPtr then
    error "newDictionary returned NULL."
    else fmap Dictionary $ newForeignPtr _freeDictionary dict

-- freeDictionary :: Dictionary -> IO ()
-- freeDictionary dict = do
--   _freeDictionary $ unDict dict
--   return ()
