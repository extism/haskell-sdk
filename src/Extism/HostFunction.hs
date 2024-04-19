{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Extism.HostFunction
  ( CurrentPlugin (..),
    ValType (..),
    Val (..),
    MemoryHandle,
    Function,
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
    hostFunction',
    input,
    output,
    getParams,
    setResults,
    ptr,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, unsafePackLenAddress)
import Data.IORef
import Data.Word
import Extism
import Extism.Bindings
import Extism.Encoding
import Foreign.C.String
import Foreign.Concurrent
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import GHC.Ptr

ptr :: ValType
ptr = I64

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
memoryBytes :: CurrentPlugin -> MemoryHandle -> IO B.ByteString
memoryBytes plugin offs = do
  Ptr ptr <- memoryOffset plugin offs
  len <- memoryLength plugin offs
  BS.unsafePackLenAddress (fromIntegral len) ptr

-- | Access the data associated with a handle as a 'String'
memoryString :: CurrentPlugin -> MemoryHandle -> IO String
memoryString plugin offs = do
  fromByteString <$> memoryBytes plugin offs

-- | Access the data associated with a handle and convert it into a Haskell type
memoryGet :: (FromBytes a) => CurrentPlugin -> MemoryHandle -> IO (Result a)
memoryGet plugin offs = do
  x <- memoryBytes plugin offs
  return $ fromBytes x

-- | Access the data associated with a handle and convert it into a Haskell type
memoryGet' :: (FromBytes a) => CurrentPlugin -> MemoryHandle -> IO a
memoryGet' plugin offs = do
  x <- memoryBytes plugin offs
  return $ unwrap $ fromBytes x

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

alloc :: (ToBytes a) => CurrentPlugin -> a -> IO MemoryHandle
alloc plugin x =
  let a = toBytes x
   in allocBytes plugin a

-- | Create a new I32 'Val'
toI32 :: (Integral a) => a -> Val
toI32 x = ValI32 (fromIntegral x)

-- | Create a new I64 'Val'
toI64 :: (Integral a) => a -> Val
toI64 x = ValI64 (fromIntegral x)

-- | Create a new F32 'Val'
toF32 :: Float -> Val
toF32 = ValF32

-- | Create a new F64 'Val'
toF64 :: Double -> Val
toF64 = ValF64

-- | Get I32 'Val'
fromI32 :: (Integral a) => Val -> Maybe a
fromI32 (ValI32 x) = Just (fromIntegral x)
fromI32 _ = Nothing

-- | Get I64 'Val'
fromI64 :: (Integral a) => Val -> Maybe a
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

output :: (ToBytes a) => CurrentPlugin -> Int -> a -> IO ()
output !p !index !x =
  do
    mem <- alloc p x
    if index >= len
      then return ()
      else pokeElemOff res index (toI64 mem)
  where
    CurrentPlugin _ _ !res !len = p

input :: (FromBytes a) => CurrentPlugin -> Int -> IO (Result a)
input plugin index =
  case x of
    Nothing -> return $ Left (ExtismError "invalid parameter")
    Just offs -> do
      memoryGet plugin (MemoryHandle offs)
  where
    (CurrentPlugin _ params _ _) = plugin
    x = fromI64 (params !! index) :: Maybe Word64

input' :: (FromBytes a) => CurrentPlugin -> Int -> IO a
input' plugin index =
  unwrap <$> input plugin index

callback :: (CurrentPlugin -> a -> IO ()) -> (Ptr ExtismCurrentPlugin -> Ptr Val -> Word64 -> Ptr Val -> Word64 -> Ptr () -> IO ())
callback f plugin params nparams results nresults ptr = do
  p <- peekArray (fromIntegral nparams) params
  (userData, _, _, _) <- deRefStablePtr (castPtrToStablePtr ptr)
  f (CurrentPlugin plugin p results (fromIntegral nresults)) userData

hostFunctionWithNamespace' ns name params results f v =
  do
    let g = callback f
    cb <- callbackWrap g
    free <- freePtrWrap freePtr
    userData <- newStablePtr (v, free, cb, g)
    let userDataPtr = castStablePtrToPtr userData
    x <- withCString name $ \name' ->
      withArray params $ \params' ->
        withArray results $ \results' ->
          extism_function_new name' params' nparams results' nresults cb userDataPtr free
    let freeFn = extism_function_free x
    case ns of
      Nothing -> return ()
      Just ns -> withCString ns (extism_function_set_namespace x)
    fptr <- Foreign.Concurrent.newForeignPtr x freeFn
    return $ Function fptr (castPtrToStablePtr userDataPtr)
  where
    nparams = fromIntegral $ length params
    nresults = fromIntegral $ length results

-- | 'hostFunction "function_name" inputTypes outputTypes callback userData' creates a new
-- | 'Function' in the default namespace that can be called from a 'Plugin'
hostFunction :: String -> [ValType] -> [ValType] -> (CurrentPlugin -> a -> IO ()) -> a -> IO Function
hostFunction = hostFunctionWithNamespace' Nothing

-- | 'hostFunction' "namespace" "function_name" inputTypes outputTypes callback userData' creates a new
-- | 'Function' in the provided namespace that can be called from a 'Plugin'
hostFunction' :: String -> String -> [ValType] -> [ValType] -> (CurrentPlugin -> a -> IO ()) -> a -> IO Function
hostFunction' ns = hostFunctionWithNamespace' (Just ns)
