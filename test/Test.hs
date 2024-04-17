import Extism
import Extism.HostFunction
import Extism.Manifest
import Extism.Pool
import Test.HUnit

assertUnwrap (Right x) = return x
assertUnwrap (Left (ExtismError msg)) =
  assertFailure msg

defaultManifest = manifest [wasmFile "../wasm/code.wasm"]

hostFunctionManifest = manifest [wasmFile "../wasm/code-functions.wasm"]

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
  assertEqual "count vowels output" "{\"count\": 4}" res

pluginCall = do
  p <- initPlugin
  checkCallResult p

hello plugin () = do
  s <- unwrap <$> input plugin 0
  assertEqual "host function input" "{\"count\": 4}" s
  putStrLn "Hello from Haskell!"
  output plugin 0 "{\"count\": 999}"

pluginCallHostFunction = do
  p <- Extism.newPlugin hostFunctionManifest [] False >>= assertUnwrap
  res <- call p "count_vowels" "this is a test" >>= assertUnwrap
  assertEqual "count vowels output" "{\"count\": 999}" res

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

testPool = do
  pool <- newPool

t name f = TestLabel name (TestCase f)

main = do
  runTestTT
    ( TestList
        [ t "Plugin.FunctionExists" pluginFunctionExists,
          t "Plugin.Call" pluginCall,
          t "Plugin.CallHostFunction" pluginCallHostFunction,
          t "Plugin.Multiple" pluginMultiple,
          t "Plugin.Config" pluginConfig,
          t
            "SetLogFile"
            testSetLogFile
            t
            "Pool"
            testPool
        ]
    )
