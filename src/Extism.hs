{-# LANGUAGE FlexibleInstances #-}

module Extism (
  module Extism.Manifest,
  Function(..),
  Plugin(..),
  CancelHandle(..),
  LogLevel(..),
  Error(..),
  Result(..),
  toByteString,
  fromByteString,
  extismVersion,
  newPlugin,
  pluginFromManifest,
  isValid,
  setConfig,
  setLogFile,
  functionExists,
  call,
  cancelHandle,
  cancel,
  pluginID,
  unwrap,
  ToBytes(..),
  Encoding,
  FromBytes(..),
  JSONValue(..)
) where

import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Ptr
import GHC.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.StablePtr
import Foreign.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (c2w, w2c, unsafePackLenAddress)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Text.JSON (encode, decode, toJSObject, showJSON, Result(..))
import qualified Extism.JSON (JSON(..))
import Extism.Manifest (Manifest, toString)
import Extism.Bindings
import qualified Data.UUID (UUID, fromByteString)

-- | Host function, see 'Extism.HostFunction.hostFunction'
data Function = Function (ForeignPtr ExtismFunction) (StablePtr ()) deriving Eq

-- | Plugins can be used to call WASM function
newtype Plugin = Plugin (ForeignPtr ExtismPlugin) deriving Eq

-- | Cancellation handle for Plugins
newtype CancelHandle = CancelHandle (Ptr ExtismCancelHandle)

-- | Log level
data LogLevel = LogError | LogWarn | LogInfo | LogDebug | LogTrace deriving (Show, Eq)

-- | Extism error
newtype Error = ExtismError String deriving (Show, Eq)

-- | Result type
type Result a = Either Error a

-- | Helper function to convert a 'String' to a 'ByteString'
toByteString :: String -> B.ByteString
toByteString x = B.pack (map c2w x)

-- | Helper function to convert a 'ByteString' to a 'String'
fromByteString :: B.ByteString -> String
fromByteString bs = map w2c $ B.unpack bs

-- | Get the Extism version string
extismVersion :: () -> IO String
extismVersion () = do
  v <- extism_version
  peekCString v

-- | Create a 'Plugin' from a WASM module, `useWasi` determines if WASI should
-- | be linked
newPlugin :: B.ByteString -> [Function] -> Bool -> IO (Result Plugin)
newPlugin wasm functions useWasi =
  let nfunctions = fromIntegral (length functions) in
  let length' = fromIntegral (B.length wasm) in
  let wasi = fromInteger (if useWasi then 1 else 0) in
  do
    funcs <- mapM (\(Function ptr _) -> withForeignPtr ptr return) functions
    alloca (\e-> do
      let errmsg = (e :: Ptr CString)
      p <- unsafeUseAsCString wasm (\s ->
        withArray funcs (\funcs ->
          extism_plugin_new (castPtr s) length' funcs nfunctions wasi errmsg ))
      if p == nullPtr then do
        err <- peek errmsg
        e <- peekCString err 
        extism_plugin_new_error_free err
        return $ Left (ExtismError e)
      else do
        ptr <- Foreign.Concurrent.newForeignPtr p (extism_plugin_free p)
        return $ Right (Plugin ptr))

-- | Create a 'Plugin' from a 'Manifest'
pluginFromManifest :: Manifest -> [Function] -> Bool -> IO (Result Plugin)
pluginFromManifest manifest functions useWasi =
  let wasm = toByteString $ toString manifest in
  newPlugin wasm functions useWasi

-- | Check if a 'Plugin' is valid
isValid :: Plugin -> IO Bool
isValid (Plugin p) = withForeignPtr p (\x -> return (x /= nullPtr))

-- | Set configuration values for a plugin
setConfig :: Plugin -> [(String, Maybe String)] -> IO Bool
setConfig (Plugin plugin) x =
  let obj = Text.JSON.toJSObject [(k, Text.JSON.showJSON v) | (k, v) <- x] in
  let bs = toByteString (Text.JSON.encode obj) in
  let length' = fromIntegral (B.length bs) in
  unsafeUseAsCString bs (\s ->
    withForeignPtr plugin (\plugin'-> do
      b <- extism_plugin_config plugin' (castPtr s) length'
      return $ b /= 0))

levelStr LogError = "error"
levelStr LogDebug = "debug"
levelStr LogWarn = "warn"
levelStr LogTrace = "trace"
levelStr LogInfo = "info"

-- | Set the log file and level, this is a global configuration
setLogFile :: String -> LogLevel -> IO Bool
setLogFile filename level =
  let s = levelStr level in
  withCString filename (\f ->
    withCString s (\l -> do
      b <- extism_log_file f l
      return $ b /= 0))

-- | Check if a function exists in the given plugin
functionExists :: Plugin -> String -> IO Bool
functionExists (Plugin plugin) name =
  withForeignPtr plugin (\plugin' -> do
    b <- withCString name (extism_plugin_function_exists plugin')
    if b == 1 then return True else return False)

-- Used to convert a value into linear memory
class ToBytes a where
  toBytes :: a -> B.ByteString

-- Used to read a value from linear memory
class FromBytes a where
  fromBytes :: B.ByteString -> Result a

-- Encoding is used to indicate a type implements both `ToBytes` and `FromBytes`
class (ToBytes a, FromBytes a) => Encoding a where

instance ToBytes B.ByteString where
  toBytes x = x

instance FromBytes B.ByteString where
  fromBytes b = Right b

instance ToBytes [Char] where
  toBytes = toByteString

instance FromBytes [Char] where
  fromBytes bs = 
    Right $ fromByteString bs

-- Wraps a `JSON` value for input/output
newtype JSONValue x = JSONValue x

instance Extism.JSON.JSON a => ToBytes (JSONValue a)  where
  toBytes (JSONValue x) =
    toByteString $ Text.JSON.encode x

   
instance Extism.JSON.JSON a => FromBytes (JSONValue a) where
  fromBytes bs = do
    case Text.JSON.decode (fromByteString bs) of
      Text.JSON.Error x -> Left (ExtismError x)
      Text.JSON.Ok x -> Right (JSONValue x)


--- | Call a function provided by the given plugin
call :: (ToBytes a, FromBytes b) => Plugin -> String -> a -> IO (Result b)
call (Plugin plugin) name inp =
  let input = toBytes inp in
  let length' = fromIntegral (B.length input) in
  withForeignPtr plugin (\plugin' -> do
    rc <- withCString name (\name' ->
      unsafeUseAsCString input (\input' ->
        extism_plugin_call plugin' name' (castPtr input') length'))
    err <- extism_error plugin'
    if err /= nullPtr
      then do e <- peekCString err
              return $ Left (ExtismError e)
    else if rc == 0
      then do
        len <- extism_plugin_output_length plugin'
        Ptr ptr <- extism_plugin_output_data plugin'
        x <- unsafePackLenAddress (fromIntegral len) ptr
        return $ fromBytes x
    else return $ Left (ExtismError "Call failed"))

-- | Create a new 'CancelHandle' that can be used to cancel a running plugin
-- | from another thread.
cancelHandle :: Plugin -> IO CancelHandle
cancelHandle (Plugin plugin) = do
  handle <- withForeignPtr plugin extism_plugin_cancel_handle
  return (CancelHandle handle)

-- | Cancel a running plugin using a 'CancelHandle'
cancel :: CancelHandle -> IO Bool
cancel (CancelHandle handle) =
  extism_plugin_cancel handle

pluginID :: Plugin -> IO Data.UUID.UUID
pluginID (Plugin plugin) =
  withForeignPtr plugin (\plugin' -> do
    ptr <- extism_plugin_id plugin'
    buf <- B.packCStringLen (castPtr ptr, 16)
    case Data.UUID.fromByteString (BL.fromStrict buf) of
      Nothing -> error "Invalid Plugin ID" 
      Just x -> return x)

    
unwrap (Right x) = x
unwrap (Left (ExtismError msg)) =
  error msg
