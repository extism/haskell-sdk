# Extism

Haskell Host SDK for Extism

## Documentation

Documentation is available on [Hackage](https://hackage.haskell.org/package/extism)

## Example

```haskell
module Main where

import Extism
import Extism.HostFunction
import Extism.Manifest(manifest, wasmFile)

-- Host function, prints a greeting then modifies the vowel count
hello currPlugin params msg = do
  putStrLn "Hello from Haskell!"

  -- Print userdata
  putStrLn msg

  -- Allocate and return a string
  offs <- alloc currPlugin "{\"count\": 999}"
  return [toI64 offs]

main = do
  setLogFile "stdout" LogError

  -- Create a manifest with the WebAssembly file
  let m = manifest [wasmFile "wasm/code-functions.wasm"]

  -- Create a host function named "hello_world"
  f <- hostFunction "hello_world" [I64] [I64] hello "Hello, again"

  -- Load the plugin
  plugin <- unwrap <$> pluginFromManifest m [f] True

  -- Call the "count_vowels" function
  res <- unwrap <$> call plugin "count_vowels" "this is a test"

  -- Print the results
  putStrLn res
```
