{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module Morphy.DAWGDict
  ( Dictionary
  , newDictionary
  , readDictionaryFromFile
  )
  where

import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.String (CString, withCString)

data DictionaryClass

newtype Dictionary = Dictionary {unDict :: ForeignPtr DictionaryClass}
  deriving Show

foreign import ccall unsafe "hsdawgdic.h newDictionary"
   _newDictionary :: IO (Ptr DictionaryClass)
foreign import ccall unsafe "hsdawgdic.h &freeDictionary"
   _freeDictionary :: FunPtr(Ptr DictionaryClass -> IO ())
foreign import ccall unsafe "hsdawgdic.h readDictionaryFromFile"
   _readDictionaryFromFile :: Ptr DictionaryClass -> CString -> IO Int

newDictionary :: IO Dictionary
newDictionary = do
  dict <- _newDictionary
  if dict == nullPtr then
    error "newDictionary returned NULL."
    else fmap Dictionary $ newForeignPtr _freeDictionary dict

withDictionaryPtr :: Dictionary -> (Ptr DictionaryClass -> IO ()) -> IO ()
withDictionaryPtr dict f =
  let ffdict = unDict dict
  in
    withForeignPtr ffdict (\dictPtr -> f dictPtr )

readDictionaryFromFile :: Dictionary -> String -> IO ()
readDictionaryFromFile dict fileName = do
  withCString fileName
    (\cfn ->
       withDictionaryPtr dict
         (\dictPtr -> do
             rc <- _readDictionaryFromFile dictPtr cfn
             if rc /= (0::Int)
               then return ()
               else error "Couldn't read dictionary" ))

-- freeDictionary :: Dictionary -> IO ()
-- freeDictionary dict = do
--   _freeDictionary $ unDict dict
--   return ()
