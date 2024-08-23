module Extism.Manifest where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS (unpack)
import Extism.JSON
import Text.JSON
import Text.JSON.Generic

-- | Memory options
data Memory = Memory
  { memoryMaxPages :: Nullable Int,
    memoryMaxHttpResponseBytes :: Nullable Int,
    memoryMaxVarBytes :: Nullable Int
  }
  deriving (Eq, Show)

instance JSON Memory where
  showJSON (Memory max maxHttp maxVar) =
    object
      [ "max_pages" .= max,
        "max_http_response_bytes" .= maxHttp,
        "max_var_bytes" .= maxVar
      ]
  readJSON obj =
    let max = obj .? "max_pages"
        httpMax = obj .? "max_http_response_bytes"
        maxVar = obj .? "max_var_bytes"
     in Ok (Memory max httpMax maxVar)

-- | HTTP request
data HTTPRequest = HTTPRequest
  { url :: String,
    headers :: Nullable [(String, String)],
    method :: Nullable String
  }
  deriving (Eq, Show)

makeKV x =
  object [(k, showJSON v) | (k, v) <- x]

requestObj (HTTPRequest url headers method) =
  [ "url" .= url,
    "headers" .= mapNullable makeKV headers,
    "method" .= method
  ]

instance JSON HTTPRequest where
  showJSON req = object $ requestObj req
  readJSON x =
    let url = x .? "url"
        headers = x .? "headers"
        method = x .? "method"
     in case url of
          Null -> Error "Missing 'url' field"
          NotNull url -> Ok (HTTPRequest url headers method)

-- | WASM from file
data WasmFile = WasmFile
  { filePath :: String,
    fileName :: Nullable String,
    fileHash :: Nullable String
  }
  deriving (Eq, Show)

instance JSON WasmFile where
  showJSON (WasmFile path name hash) =
    object
      [ "path" .= path,
        "name" .= name,
        "hash" .= hash
      ]
  readJSON x =
    let path = x .? "url"
        name = x .? "name"
        hash = x .? "hash"
     in case path of
          Null -> Error "Missing 'path' field"
          NotNull path -> Ok (WasmFile path name hash)

-- | WASM from raw bytes
data WasmData = WasmData
  { dataBytes :: Base64,
    dataName :: Nullable String,
    dataHash :: Nullable String
  }
  deriving (Eq, Show)

instance JSON WasmData where
  showJSON (WasmData bytes name hash) =
    object
      [ "data" .= bytes,
        "name" .= name,
        "hash" .= hash
      ]
  readJSON x =
    let d = x .? "data"
        name = x .? "name"
        hash = x .? "hash"
     in case d of
          Null -> Error "Missing 'path' field"
          NotNull d ->
            case readJSON d of
              Error msg -> Error msg
              Ok d' -> Ok (WasmData d' name hash)

-- | WASM from a URL
data WasmURL = WasmURL
  { req :: HTTPRequest,
    urlName :: Nullable String,
    urlHash :: Nullable String
  }
  deriving (Eq, Show)

instance JSON WasmURL where
  showJSON (WasmURL req name hash) =
    object
      ( "name" .= name
          : "hash" .= hash
          : requestObj req
      )
  readJSON x =
    let req = x .? "req"
        name = x .? "name"
        hash = x .? "hash"
     in case fromNullable req of
          Nothing -> Error "Missing 'req' field"
          Just req -> Ok (WasmURL req name hash)

-- | Specifies where to get WASM module data
data Wasm = File WasmFile | Data WasmData | URL WasmURL deriving (Eq, Show)

instance JSON Wasm where
  showJSON x =
    case x of
      File f -> showJSON f
      Data d -> showJSON d
      URL u -> showJSON u
  readJSON x =
    case file of
      Ok x -> Ok (File x)
      Error _ ->
        let data' = (readJSON x :: Result WasmData)
         in case data' of
              Ok x -> Ok (Data x)
              Error _ ->
                let url = (readJSON x :: Result WasmURL)
                 in case url of
                      Ok x -> Ok (URL x)
                      Error _ -> Error "JSON does not match any of the Wasm types"
    where
      file = readJSON x :: Result WasmFile

wasmFile :: String -> Wasm
wasmFile path =
  File WasmFile {filePath = path, fileName = null', fileHash = null'}

wasmURL :: String -> String -> Wasm
wasmURL method url =
  let r = HTTPRequest {url = url, headers = null', method = nonNull method}
   in URL WasmURL {req = r, urlName = null', urlHash = null'}

wasmData :: B.ByteString -> Wasm
wasmData d =
  Data WasmData {dataBytes = Base64 d, dataName = null', dataHash = null'}

withName :: Wasm -> String -> Wasm
withName (Data d) name = Data d {dataName = nonNull name}
withName (URL url) name = URL url {urlName = nonNull name}
withName (File f) name = File f {fileName = nonNull name}

withHash :: Wasm -> String -> Wasm
withHash (Data d) hash = Data d {dataHash = nonNull hash}
withHash (URL url) hash = URL url {urlHash = nonNull hash}
withHash (File f) hash = File f {fileHash = nonNull hash}

-- | The 'Manifest' type is used to provide WASM data and configuration to the
-- | Extism runtime
data Manifest = Manifest
  { wasm :: [Wasm],
    memory :: Nullable Memory,
    config :: Nullable [(String, String)],
    allowedHosts :: Nullable [String],
    allowedPaths :: Nullable [(String, String)],
    timeout :: Nullable Int
  }
  deriving (Eq, Show)

instance JSON Manifest where
  showJSON (Manifest wasm memory config hosts paths timeout) =
    let w = makeArray wasm
     in object
          [ "wasm" .= w,
            "memory" .= memory,
            "config" .= mapNullable makeKV config,
            "allowed_hosts" .= hosts,
            "allowed_paths" .= mapNullable makeKV paths,
            "timeout_ms" .= timeout
          ]
  readJSON x =
    let wasm = x .? "wasm"
        memory = x .? "memory"
        config = x .? "config"
        hosts = x .? "allowed_hosts"
        paths = x .? "allowed_paths"
        timeout = x .? "timeout_ms"
     in case fromNullable wasm of
          Nothing -> Error "Missing 'wasm' field"
          Just wasm -> Ok (Manifest wasm memory config hosts paths timeout)

-- | Create a new 'Manifest' from a list of 'Wasm'
manifest :: [Wasm] -> Manifest
manifest wasm =
  Manifest
    { wasm = wasm,
      memory = null',
      config = null',
      allowedHosts = null',
      allowedPaths = null',
      timeout = null'
    }

-- | Update the config values
withConfig :: Manifest -> [(String, String)] -> Manifest
withConfig m config =
  m {config = nonNull config}

-- | Update allowed hosts for `extism_http_request`
withHosts :: Manifest -> [String] -> Manifest
withHosts m hosts =
  m {allowedHosts = nonNull hosts}

-- | Update allowed paths
withPaths :: Manifest -> [(String, String)] -> Manifest
withPaths m p =
  m {allowedPaths = nonNull p}

-- | Update plugin timeout (in milliseconds)
withTimeout :: Manifest -> Int -> Manifest
withTimeout m t =
  m {timeout = nonNull t}

-- | Set memory.max_pages
withMaxPages :: Manifest -> Int -> Manifest
withMaxPages m pages =
  case memory m of
    Null ->
      m {memory = NotNull $ Memory (NotNull pages) Null Null}
    NotNull (Memory _ x y) ->
      m {memory = NotNull $ Memory (NotNull pages) x y}

-- | Set memory.max_http_response_bytes
withMaxHttpResponseBytes :: Manifest -> Int -> Manifest
withMaxHttpResponseBytes m max =
  case memory m of
    Null ->
      m {memory = NotNull $ Memory Null (NotNull max) Null}
    NotNull (Memory x _ y) ->
      m {memory = NotNull $ Memory x (NotNull max) y}

-- | Set memory.max_var_bytes
withMaxVarBytes :: Manifest -> Int -> Manifest
withMaxVarBytes m max =
  case memory m of
    Null ->
      m {memory = NotNull $ Memory Null Null (NotNull max)}
    NotNull (Memory x y _) ->
      m {memory = NotNull $ Memory x y (NotNull max)}

fromString :: String -> Either String Manifest
fromString s = do
  let x = decode s
  resultToEither x

fromFile :: FilePath -> IO (Either String Manifest)
fromFile path = do
  s <- readFile path
  return $ fromString s

toString :: Manifest -> String
toString = encode

toFile :: FilePath -> Manifest -> IO ()
toFile path m = do
  writeFile path (toString m)
