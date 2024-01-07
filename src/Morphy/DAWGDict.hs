{-# Language ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module Morphy.DAWGDict
  ( Dictionary
  , newDictionary
  , readDictionaryFromFile
  , followDictionary
  , valueDictionary
  )
  where

import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt, CUInt)
import qualified Data.ByteString.Lazy.UTF8 as U8
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Encoding.Types (TextEncoding)
import System.IO (utf8)
import qualified GHC.Foreign as GF
import qualified Data.ByteString.Lazy.UTF8 as BLU

data DictionaryClass

newtype Dictionary = Dictionary {unDict :: ForeignPtr DictionaryClass}
  deriving Show

foreign import ccall unsafe "hsdawgdic.h newDictionary"
   _newDictionary :: IO (Ptr DictionaryClass)
foreign import ccall unsafe "hsdawgdic.h &freeDictionary"
   _freeDictionary :: FunPtr(Ptr DictionaryClass -> IO ())
foreign import ccall unsafe "hsdawgdic.h readDictionaryFromFile"
   _readDictionaryFromFile :: Ptr DictionaryClass -> CString -> IO Int
foreign import ccall unsafe "hsdawgdic.h followDictionary"
   _followDictionary :: Ptr DictionaryClass -> CString -> Ptr CUInt -> IO Int
-- foreign import ccall unsafe "hsdawgdic.h valueDictionary"
--   _valueDictionary :: Ptr DictionaryClass -> Int -> CString -> Int -> IO Int
foreign import ccall unsafe "hsdawgdic.h startCompleter"
   _startCompleter :: Ptr DictionaryClass -> Int -> IO ()
foreign import ccall unsafe "hsdawgdic.h nextCompleter"
   _nextCompleter :: Ptr DictionaryClass -> IO (Int)
foreign import ccall unsafe "hsdawgdic.h keyCompleter"
   _keyCompleter :: Ptr DictionaryClass -> CString -> Int -> IO ()
foreign import ccall unsafe "hsdawgdic.h lengthCompleter"
   _lengthCompleter :: Ptr DictionaryClass -> IO (Int)
foreign import ccall unsafe "hsdawgdic.h valueCompleter"
   _valueCompleter :: Ptr DictionaryClass -> IO (Int)


newDictionary :: IO Dictionary
newDictionary = do
  dict <- _newDictionary
  if dict == nullPtr then
    error "newDictionary returned NULL."
    else fmap Dictionary $ newForeignPtr _freeDictionary dict

withDictionaryPtr :: Dictionary -> (Ptr DictionaryClass -> IO a) -> IO a
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

followDictionary' :: Dictionary -> String -> Int -> IO (Maybe Int)
followDictionary' dict str index = do
  GF.withCString utf8 str
    (\cstr ->
       withDictionaryPtr dict
         (\dictPtr ->
             alloca
               (\indexptr -> do
                   poke indexptr ((fromIntegral index) :: CUInt)
                   rc <- _followDictionary dictPtr cstr indexptr
                   if rc /= (0::Int)
                     then do
                       val <- peek indexptr
                       return . Just . fromIntegral $ val
                     else return Nothing)))

followDictionary :: Dictionary -> String -> Int -> Maybe Int
followDictionary dict str index = unsafePerformIO $ followDictionary' dict str index

valueDictionary'' :: Ptr DictionaryClass -> Int -> CString -> Int -> IO [(String, Int)]
valueDictionary'' dictPtr index strPtr strSize = do
  _startCompleter dictPtr index
  go
  where
    go :: IO ([(String, Int)])
    go = do
      rc <- _nextCompleter dictPtr
      if rc /= (0::Int)
        then do
          _keyCompleter dictPtr strPtr strSize
          len <- _lengthCompleter dictPtr
          str <- GF.peekCStringLen utf8 (strPtr, len)
          val <- _valueCompleter dictPtr
          let x = (str, val)
          xs <- go
          return (x:xs)
        else
          return []



valueDictionary' :: Dictionary -> Int -> IO [(String, Int)]
valueDictionary' dict index = do
  withDictionaryPtr dict
    (\dictPtr ->
        go dictPtr index)
  where
    go dictPtr index = do
      let maxSize = 2048
      allocaBytes maxSize
        (\ptr -> do
            valueDictionary'' dictPtr index ptr maxSize)


-- return . Just . BLU.fromString $

valueDictionary :: Dictionary -> Int -> [(String, Int)]
valueDictionary dict index = unsafePerformIO $ valueDictionary' dict index


-- freeDictionary :: Dictionary -> IO ()
-- freeDictionary dict = do
--   _freeDictionary $ unDict dict
--   return ()
