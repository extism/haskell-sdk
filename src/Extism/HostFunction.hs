{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, BangPatterns #-}

module Extism.HostFunction(
  CurrentPlugin(..),
  ValType(..),
  Val(..),
  MemoryHandle,
  memoryAlloc,
  memoryLength,
  memoryFree,
  memory,
  memoryOffset,
  memoryBytes,
  memoryString,
  memoryGet,
  allocBytes,
  allocString,
  alloc,
  toI32,
  toI64,
  toF32,
  toF64,
  fromI32,
  fromI64,
  fromF32,
  fromF64,
  hostFunction,
  input,
  output,
  getParams,
  setResults
) where

import Extism
import Extism.Bindings
import Data.Word
import qualified Data.ByteString as B
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Foreign.StablePtr
import Foreign.Concurrent
import Foreign.Marshal.Array
import GHC.Ptr
import qualified Data.ByteString.Internal as BS (c2w, unsafePackLenAddress)
import Data.IORef

-- | Access the plugin that is currently executing from inside a host function
data CurrentPlugin = CurrentPlugin (Ptr ExtismCurrentPlugin) [Val] (Ptr Val) Int

-- | A memory handle represents an allocated block of Extism memory
newtype MemoryHandle = MemoryHandle Word64 deriving (Num, Enum, Eq, Ord, Real, Integral, Show)

-- | Allocate a new handle of the given size
memoryAlloc :: CurrentPlugin -> Word64 -> IO MemoryHandle
memoryAlloc (CurrentPlugin p _ _ _) n = MemoryHandle <$> extism_current_plugin_memory_alloc p n

-- | Get the length of a handle, returns 0 if the handle is invalid
memoryLength :: CurrentPlugin -> MemoryHandle -> IO Word64
memoryLength (CurrentPlugin p _ _ _) (MemoryHandle offs) = extism_current_plugin_memory_length p offs

-- | Free allocated memory
memoryFree :: CurrentPlugin -> MemoryHandle -> IO ()
memoryFree (CurrentPlugin p _ _ _) (MemoryHandle offs) = extism_current_plugin_memory_free p offs

-- | Access a pointer to the entire memory region
memory :: CurrentPlugin -> IO (Ptr Word8)
memory (CurrentPlugin p _ _ _) = extism_current_plugin_memory p

-- | Access the pointer for the given 'MemoryHandle'
memoryOffset :: CurrentPlugin -> MemoryHandle -> IO (Ptr Word8)
memoryOffset (CurrentPlugin plugin _ _ _) (MemoryHandle offs) = do
  x <- extism_current_plugin_memory plugin
  return $ plusPtr x (fromIntegral offs)

-- | Access the data associated with a handle as a 'ByteString'
memoryBytes :: CurrentPlugin -> MemoryHandle ->  IO B.ByteString
memoryBytes plugin offs = do
  Ptr ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  BS.unsafePackLenAddress (fromIntegral len) ptr

  
-- | Access the data associated with a handle as a 'String'
memoryString :: CurrentPlugin -> MemoryHandle ->  IO String
memoryString plugin offs = do
  fromByteString <$> memoryBytes plugin offs

-- | Access the data associated with a handle and convert it into a Haskell type
memoryGet :: FromPointer a => CurrentPlugin -> MemoryHandle -> IO (Result a)
memoryGet plugin offs = do
  ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  fromPointer (castPtr ptr) (fromIntegral len)

-- | Allocate memory and copy an existing 'ByteString' into it
allocBytes :: CurrentPlugin -> B.ByteString -> IO MemoryHandle
allocBytes plugin s = do
  let length = B.length s
  offs <- memoryAlloc plugin (fromIntegral length)
  ptr <- memoryOffset plugin offs
  pokeArray ptr (B.unpack s)
  return offs

  
-- | Allocate memory and copy an existing 'String' into it
allocString :: CurrentPlugin -> String -> IO MemoryHandle
allocString plugin s = do
  let length = Prelude.length s
  offs <- memoryAlloc plugin (fromIntegral length)
  ptr <- memoryOffset plugin offs
  pokeArray ptr (Prelude.map BS.c2w s)
  return offs

  
alloc :: ToBytes a => CurrentPlugin -> a -> IO MemoryHandle
alloc plugin x =
  let a = toBytes x in
  allocBytes plugin a


-- | Create a new I32 'Val'
toI32 :: Integral a => a -> Val
toI32 x = ValI32 (fromIntegral x)

-- | Create a new I64 'Val'
toI64 :: Integral a => a -> Val
toI64 x = ValI64 (fromIntegral x)

-- | Create a new F32 'Val'
toF32 :: Float -> Val
toF32 = ValF32

-- | Create a new F64 'Val'
toF64 :: Double -> Val
toF64 = ValF64

-- | Get I32 'Val'
fromI32 :: Integral a => Val -> Maybe a
fromI32 (ValI32 x) = Just (fromIntegral x)
fromI32 _ = Nothing

-- | Get I64 'Val'
fromI64 :: Integral a => Val -> Maybe a
fromI64 (ValI64 x) = Just (fromIntegral x)
fromI64 _ = Nothing

-- | Get F32 'Val'
fromF32 :: Val -> Maybe Float
fromF32 (ValF32 x) = Just x
fromF32 _ = Nothing

-- | Get F64 'Val'
fromF64 :: Val -> Maybe Double
fromF64 (ValF64 x) = Just x
fromF64 _ = Nothing

setResults :: CurrentPlugin -> [Val] -> IO ()
setResults (CurrentPlugin _ _ res _) = pokeArray res

getParams :: CurrentPlugin -> [Val]
getParams (CurrentPlugin _ params _ _) = params

output :: ToBytes a => CurrentPlugin -> Int -> a -> IO ()
output !p !index !x =
  let 
    CurrentPlugin _ _ !res !len = p 
  in
  do
    mem <- alloc p x
    if index >= len then return ()
    else pokeElemOff res index (toI64 mem)

input :: FromPointer a => CurrentPlugin -> Int -> IO (Result a)
input plugin index =
  let (CurrentPlugin _ params _ _) = plugin in
  let x = fromI64 (params !! index) :: Maybe Word64 in
  case x of
    Nothing -> return $ Left (ExtismError "invalid parameter")
    Just offs -> do
      memoryGet plugin (MemoryHandle offs)


callback :: (CurrentPlugin -> a -> IO ()) -> (Ptr ExtismCurrentPlugin -> Ptr Val -> Word64 -> Ptr Val -> Word64 -> Ptr () -> IO ())
callback f plugin params nparams results nresults ptr = do
    p <- peekArray (fromIntegral nparams) params
    (userData, _, _)  <- deRefStablePtr (castPtrToStablePtr ptr)
    f (CurrentPlugin plugin p results (fromIntegral nresults)) userData

-- | Create a new 'Function' that can be called from a 'Plugin'
hostFunction :: String -> [ValType] -> [ValType] -> (CurrentPlugin -> a -> IO ()) -> a -> IO Function
hostFunction name params results f v =
  let nparams = fromIntegral $ length params in
  let nresults = fromIntegral $ length results in
  do
    cb <- callbackWrap (callback f)
    free <- freePtrWrap freePtr
    userData <- newStablePtr (v, free, cb)
    let userDataPtr = castStablePtrToPtr userData
    x <- withCString name (\name' ->
      withArray params (\params' ->
        withArray results (\results' ->
          extism_function_new name' params' nparams results' nresults cb userDataPtr free)))
    let freeFn = extism_function_free x
    fptr <- Foreign.Concurrent.newForeignPtr x freeFn
    return $ Function fptr (castPtrToStablePtr userDataPtr)

