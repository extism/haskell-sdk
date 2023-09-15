module Main where

import Extism
import Extism.HostFunction
import Extism.Manifest(manifest, wasmFile)

hello currPlugin params msg = do
  putStrLn "Hello from Haskell!"
  putStrLn msg
  offs <- alloc currPlugin "{\"count\": 999}"
  return [toI64 offs]

main = do
  setLogFile "stdout" LogError
  let m = manifest [wasmFile "wasm/code-functions.wasm"]
  f <- hostFunction "hello_world" [I64] [I64] hello "Hello, again"
  plugin <- unwrap <$> pluginFromManifest m [f] True
  id <- pluginID plugin
  print id
  res <- unwrap <$> call plugin "count_vowels" "this is a test"
  putStrLn res
