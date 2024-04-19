-- |
-- A Haskell Extism host
--
-- Requires a libextism installation, see [https://extism.org/docs/install](https://extism.org/docs/install)
module Extism
  ( module Extism.Manifest,
    module Extism.Encoding,
    Function (..),
    Plugin (..),
    CancelHandle (..),
    LogLevel (..),
    Error (..),
    Result (..),
    extismVersion,
    newPlugin,
    isValid,
    setConfig,
    setLogFile,
    functionExists,
    call,
    cancelHandle,
    cancel,
    pluginID,
    unwrap,
    ToBytes (..),
    FromBytes (..),
    JSON (..),
    PluginInput (..),
    reset,
  )
where

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, unsafePackLenAddress, w2c)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Int
import qualified Data.UUID (UUID, fromByteString, toString)
import Data.Word
import Extism.Bindings
import Extism.Encoding
import Extism.Manifest (Manifest)
import Foreign.C.String
import Foreign.Concurrent
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import GHC.Ptr
import qualified Text.JSON (Result (..), decode, encode, showJSON, toJSObject)

-- | Host function, see 'Extism.HostFunction.hostFunction'
data Function = Function (ForeignPtr ExtismFunction) (StablePtr ()) deriving (Eq)

-- | Plugins can be used to call WASM function
newtype Plugin = Plugin (ForeignPtr ExtismPlugin) deriving (Eq, Show)

-- | Cancellation handle for Plugins
newtype CancelHandle = CancelHandle (Ptr ExtismCancelHandle) deriving (Eq, Show)

-- | Log level
data LogLevel = LogError | LogWarn | LogInfo | LogDebug | LogTrace deriving (Show, Eq)

-- | Get the Extism version string
extismVersion :: () -> IO String
extismVersion () = do
  v <- extism_version
  peekCString v

-- | Defines types that can be used to pass Wasm data into a plugin
class PluginInput a where
  pluginInput :: a -> B.ByteString

instance PluginInput B.ByteString where
  pluginInput = id

instance PluginInput Manifest where
  pluginInput m = toByteString $ Text.JSON.encode m

-- | Create a 'Plugin' from a WASM module, `useWasi` determines if WASI should
-- | be linked
newPlugin :: (PluginInput a) => a -> [Function] -> Bool -> IO (Result Plugin)
newPlugin input functions useWasi = do
  funcs <- mapM (\(Function ptr _) -> withForeignPtr ptr return) functions
  alloca $ \e -> do
    let errmsg = (e :: Ptr CString)
    p <- unsafeUseAsCString wasm $ \s ->
      withArray funcs $ \funcs ->
        extism_plugin_new (castPtr s) length' funcs nfunctions wasi errmsg

    if p == nullPtr
      then do
        err <- peek errmsg
        e <- peekCString err
        extism_plugin_new_error_free err
        return $ Left (ExtismError e)
      else do
        ptr <- Foreign.Concurrent.newForeignPtr p (extism_plugin_free p)
        return $ Right (Plugin ptr)
  where
    wasm = pluginInput input
    nfunctions = fromIntegral (length functions)
    length' = fromIntegral (B.length wasm)
    wasi = fromInteger (if useWasi then 1 else 0)

-- | Same as `newPlugin` but converts the error case to an exception
newPlugin' :: (PluginInput a) => a -> [Function] -> Bool -> IO Plugin
newPlugin' input functions useWasi = do
  unwrap <$> newPlugin input functions useWasi

-- | Check if a 'Plugin' is valid
isValid :: Plugin -> IO Bool
isValid (Plugin p) = withForeignPtr p (\x -> return (x /= nullPtr))

-- | Set configuration values for a plugin
setConfig :: Plugin -> [(String, Maybe String)] -> IO Bool
setConfig (Plugin plugin) x =
  unsafeUseAsCString bs $ \s ->
    withForeignPtr plugin $ \plugin' -> do
      b <- extism_plugin_config plugin' (castPtr s) length'
      return $ b /= 0
  where
    obj = Text.JSON.toJSObject [(k, Text.JSON.showJSON v) | (k, v) <- x]
    bs = toByteString (Text.JSON.encode obj)
    length' = fromIntegral (B.length bs)

levelStr LogError = "error"
levelStr LogDebug = "debug"
levelStr LogWarn = "warn"
levelStr LogTrace = "trace"
levelStr LogInfo = "info"

-- | Set the log file and level, this is a global configuration
setLogFile :: String -> LogLevel -> IO Bool
setLogFile filename level =
  withCString filename $ \f ->
    withCString s $ \l -> do
      b <- extism_log_file f l
      return $ b /= 0
  where
    s = levelStr level

-- | Check if a function exists in the given plugin
functionExists :: Plugin -> String -> IO Bool
functionExists (Plugin plugin) name =
  withForeignPtr plugin $ \plugin' -> do
    b <- withCString name (extism_plugin_function_exists plugin')
    if b == 1 then return True else return False

--- | Call a function provided by the given plugin
call :: (ToBytes a, FromBytes b) => Plugin -> String -> a -> IO (Result b)
call (Plugin plugin) name inp =
  withForeignPtr plugin $ \plugin' -> do
    rc <- withCString name $ \name' ->
      unsafeUseAsCString input $ \input' ->
        extism_plugin_call plugin' name' (castPtr input') length'
    err <- extism_error plugin'
    if err /= nullPtr
      then do
        e <- peekCString err
        return $ Left (ExtismError e)
      else
        if rc == 0
          then do
            len <- extism_plugin_output_length plugin'
            Ptr ptr <- extism_plugin_output_data plugin'
            x <- unsafePackLenAddress (fromIntegral len) ptr
            return $ fromBytes x
          else return $ Left (ExtismError "Call failed")
  where
    input = toBytes inp
    length' = fromIntegral (B.length input)

call' :: (ToBytes a, FromBytes b) => Plugin -> String -> a -> IO b
call' plugin name inp = do
  unwrap <$> call plugin name inp

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
  withForeignPtr plugin $ \plugin' -> do
    ptr <- extism_plugin_id plugin'
    buf <- B.packCStringLen (castPtr ptr, 16)
    case Data.UUID.fromByteString (BL.fromStrict buf) of
      Nothing -> error "Invalid Plugin ID"
      Just x -> return x

reset :: Plugin -> IO ()
reset (Plugin plugin) =
  withForeignPtr plugin extism_plugin_reset

unwrap (Right x) = x
unwrap (Left (ExtismError msg)) =
  error msg
