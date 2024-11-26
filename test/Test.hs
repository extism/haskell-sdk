{-# LANGUAGE DeriveDataTypeable #-}

import Extism
import Extism.HostFunction
import Extism.JSON
import Extism.Manifest
import Test.HUnit

assertUnwrap (Right x) = return x
assertUnwrap (Left (ExtismError msg)) =
  assertFailure msg

defaultManifest = manifest [wasmFile "wasm/code.wasm"]

hostFunctionManifest = manifest [wasmFile "wasm/code-functions.wasm"]

initPlugin :: IO Plugin
initPlugin =
  Extism.newPlugin defaultManifest [] False >>= assertUnwrap

pluginFunctionExists = do
  p <- initPlugin
  exists <- functionExists p "count_vowels"
  assertBool "function exists" exists
  exists' <- functionExists p "function_doesnt_exist"
  assertBool "function doesn't exist" (not exists')

checkCallResult p = do
  res <- call p "count_vowels" "this is a test" >>= assertUnwrap
  assertEqual "count vowels output" "{\"count\":4,\"total\":4,\"vowels\":\"aeiouAEIOU\"}" res

pluginCall = do
  p <- initPlugin
  checkCallResult p

newtype Count = Count {count :: Int} deriving (Data, Show)

hello currPlugin msg = do
  putStrLn . unwrap <$> input currPlugin 0
  putStrLn "Hello from Haskell!"
  putStrLn msg
  output currPlugin 0 $ JSON $ Count 999

pluginCallHostFunction = do
  f <- newFunction "hello_world" [ptr] [ptr] "Hello, again" hello
  p <- Extism.newPlugin hostFunctionManifest [f] True >>= assertUnwrap
  res <- call p "count_vowels" "this is a test" >>= assertUnwrap
  assertEqual "count vowels output" "{\"count\":999}" res

helloContext currPlugin msg = do
  ctx <- hostContext currPlugin
  case ctx of
    Nothing -> assertBool "Expected host context" False
    Just s -> putStrLn s
  output currPlugin 0 msg

pluginCallHostContext = do
  f <- newFunction "hello_world" [ptr] [ptr] "Hello, again" helloContext
  p <- Extism.newPlugin hostFunctionManifest [f] True >>= assertUnwrap
  callWithHostContext p "count_vowels" "this is a test" "host context" >>= assertUnwrap

pluginMultiple = do
  p <- initPlugin
  checkCallResult p
  q <- initPlugin
  r <- initPlugin
  checkCallResult q
  checkCallResult r

pluginConfig = do
  p <- initPlugin
  b <- setConfig p [("a", Just "1"), ("b", Just "2"), ("c", Just "3"), ("d", Nothing)]
  assertBool "set config" b

testSetLogFile = do
  b <- setLogFile "stderr" Extism.LogError
  assertBool "set log file" b

testCompiledPlugin = do
  c <- Extism.newCompiledPlugin defaultManifest [] False >>= assertUnwrap
  p <- Extism.newPluginFromCompiled c >>= assertUnwrap
  checkCallResult p

t name f = TestLabel name (TestCase f)

main = do
  runTestTT
    ( TestList
        [ t "Plugin.FunctionExists" pluginFunctionExists,
          t "Plugin.Call" pluginCall,
          t "Plugin.CallHostFunction" pluginCallHostFunction,
          t "Plugin.CallHostContext" pluginCallHostContext,
          t "Plugin.Multiple" pluginMultiple,
          t "Plugin.Config" pluginConfig,
          t "SetLogFile" testSetLogFile,
          t "CompiledPlugin" testCompiledPlugin
        ]
    )
