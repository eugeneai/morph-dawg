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
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Encoding.Types (TextEncoding)
import System.IO (utf8)
import qualified GHC.Foreign as GF
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Split (splitOn)
import Data.ByteString.Unsafe
  (
    unsafePackMallocCStringLen
  , unsafePackCStringLen
  , unsafePackCString  )
import qualified GHC.Word as GW

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
foreign import ccall unsafe "hsdawgdic.h keyValueCompleter"
   _keyValueCompleter :: Ptr DictionaryClass -> CString -> CString -> Int -> IO ()
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

valueDictionary'' :: Ptr DictionaryClass -> Int -> CString -> CString -> Int -> (BLU.ByteString -> a) -> IO [(String, a)]
valueDictionary'' dictPtr index keyPtr valPtr strSize f = do
  _startCompleter dictPtr index
  go
  where
    go = do
      rc <- _nextCompleter dictPtr
      if rc /= (0::Int)
        then do
          _keyValueCompleter dictPtr keyPtr valPtr strSize
          -- _keyCompleter dictPtr valPtr strSize
          len <- _lengthCompleter dictPtr
          key <- GF.peekCString utf8 keyPtr
          val <- GF.peekCString utf8 valPtr
          let x = (key, f . BLU.fromString $ val)
          xs <- go
          return (x:xs)
        else
          return []
    divChar = '\x01' :: Char -- GW.Word8
    divF :: GW.Word8 -> Bool
    divF x = divChar /= '\01' -- 1 -- (1::GW.Word8)



valueDictionary' :: Dictionary -> Int -> (BLU.ByteString -> a) -> IO [(String, a)]
valueDictionary' dict index f = do
  withDictionaryPtr dict
    (\dictPtr ->
        go dictPtr index)
  where
    go dictPtr index = do
      let maxSize = 2048
      allocaBytes maxSize
        (\keyPtr -> do
            allocaBytes maxSize
              (\valPtr -> do
                  valueDictionary'' dictPtr index keyPtr valPtr maxSize f))

-- return . Just . BLU.fromString $

valueDictionary :: Dictionary -> Int -> (BLU.ByteString -> a) -> [(String, a)]
valueDictionary dict index f = unsafePerformIO $ valueDictionary' dict index f


-- freeDictionary :: Dictionary -> IO ()
-- freeDictionary dict = do
--   _freeDictionary $ unDict dict
--   return ()
