# Extism Haskell Host SDK

This repo contains the Haskell package for integrating with the [Extism](https://extism.org/) runtime.

> **Note**: If you're unsure what Extism is or what an SDK is see our homepage: [https://extism.org](https://extism.org).

## Documentation

Documentation is available at [https://hackage.haskell.org/package/extism](https://hackage.haskell.org/package/extism)

## Installation

### Install the Extism Runtime Dependency

For this library, you first need to install the Extism Runtime. You can [download the shared object directly from a release](https://github.com/extism/extism/releases) or use the [Extism CLI](https://github.com/extism/cli) to install it.

> **Note**: This library has breaking changes and targets 1.0 of the runtime. For the time being, install the runtime from our nightly development builds on git: `sudo PATH="$PATH" env extism lib install --version git`.

### Add the library to dune

Then add `extism` to your [cabal](https://www.haskell.org/cabal/) file:

```
library
  build-depends: extism
```

## Getting Started

This guide should walk you through some of the concepts in Extism and the Haskell bindings.

### Creating A Plug-in

The primary concept in Extism is the [plug-in](https://extism.org/docs/concepts/plug-in). You can think of a plug-in as a code module stored in a `.wasm` file.

Since you may not have an Extism plug-in on hand to test, let's load a demo plug-in from the web:

```haskell
module Main where
import Extism

main = do
  let wasm = wasmURL "GET" "https://github.com/extism/plugins/releases/latest/download/count_vowels.wasm"
  plugin <- unwrap <$> newPlugin (manifest [wasm]) [] True
  res <- unwrap <$> call plugin "count_vowels" "Hello, world!"
  putStrLn res
-- Prints: {"count":3,"total":3,"vowels":"aeiouAEIOU"}"
```

> **Note**: See [the Manifest docs](https://hackage.haskell.org/package/extism-manifest) as it has a rich schema and a lot of options.

This plug-in was written in Rust and it does one thing, it counts vowels in a string. As such, it exposes one "export" function: `count_vowels`. We can call exports using [Extism.call](https://hackage.haskell.org/package/extism/docs/Extism.html#v:call):

All exports have a simple interface of bytes-in and bytes-out. This plug-in happens to take a string and return a JSON encoded string with a report of results.

This library also allowes for conversion of input/outputs types using [FromBytes](https://hackage.haskell.org/package/extism/docs/Extism.html#t:FromBytes) and [ToBytes](https://hackage.haskell.org/package/extism/docs/Extism.html#t:ToBytes)

### Plug-in State

Plug-ins may be stateful or stateless. Plug-ins can maintain state b/w calls by the use of variables. Our count vowels plug-in remembers the total number of vowels it's ever counted in the "total" key in the result. You can see this by making subsequent calls to the export:

```haskell
ghci> unwrap <$> call plugin "count_vowels" "Hello, world!"
{"count":3,"total":9,"vowels":"aeiouAEIOU"}
ghci> unwrap <$> call plugin "count_vowels" "Hello, world!"
{"count":3,"total":12,"vowels":"aeiouAEIOU"}
```

These variables will persist until this plug-in is freed or you initialize a new one.

### Configuration

Plug-ins may optionally take a configuration object. This is a static way to configure the plug-in. Our count-vowels plugin takes an optional configuration to change out which characters are considered vowels. Example:

```haskell
ghci> let manifest = manifest [wasm]
ghci> plugin <- unwrap <$> newPlugin manifest [] True
ghci> res <- (unwrap <$> call plugin "count_vowels" "Yellow, world!" :: String)
ghci> res
{"count":3,"total":3,"vowels":"aeiouAEIOU"}

ghci> plugin <- withConfig (manifest [wasm]) [("vowels","aeiouyAEIOUY")] ;;
ghci> res <- (unwrap <$> call plugin "count_vowels" "Yellow, world!" :: String)
ghci> res
{"count":4,"total":4,"vowels":"aeiouAEIOUY"}
```

### Host Functions

Let's extend our count-vowels example a little bit: we can intercept the results and adjust them before returning from the plugin using a `hello_world` [host function](https://extism.org/docs/concepts/host-functions)
with `wasm/code-functions.wasm`

[Host functions](https://extism.org/docs/concepts/host-functions) allow us to grant new capabilities to our plug-ins from our application. They are simply some OCaml functions you write which can be passed down and invoked from any language inside the plug-in.

Using [Extism.HostFunction.hostFunction](https://hackage.haskell.org/package/extism/docs/Extism-HostFunction.html#v:hostFunction) we can define a host function that can be called from the guest plug-in.

In this example, we want to expose a single function to our plugin (in Haskell types): `hello_world :: String -> String` which will intercept the original result and replace it with a new one.

Let's load the manifest like usual but load up `wasm/code-functions.wasm` plug-in:

```haskell
module Main where

import Extism
import Extism.HostFunction
import Extism.Manifest (manifest, wasmFile)

hello currPlugin () = do
  putStrLn . unwrap <$> input currPlugin 0
  putStrLn "Hello from Haskell!"
  output currPlugin 0 "{\"count\": 999}"

main = do
  setLogFile "stdout" LogError -- Enable logging to stdout
  let m = manifest [wasmFile "wasm/code-functions.wasm"]
  f <- hostFunction "hello_world" [I64] [I64] hello () -- Create host function, the final argument 
                                                       -- is a userData argument that can store any
                                                       -- Haskell value 
  plugin <- unwrap <$> newPlugin m [f] True
  res <- unwrap <$> call plugin "count_vowels" "this is a test"
  putStrLn res
-- Prints: {"count": 999}
```

> *Note*: In order to write host functions you should get familiar with the methods on the [Extism.HostFunction](https://hackage.haskell.org/package/extism/docs/Extism-HostFunction.html) module.
