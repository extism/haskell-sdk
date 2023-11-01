{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Extism
import Extism.HostFunction
import Extism.JSON
import Extism.Manifest (manifest, wasmFile)

newtype Count = Count {count :: Int} deriving (Data, Typeable, Show)

hello currPlugin msg = do
  putStrLn . unwrap <$> input currPlugin 0
  putStrLn "Hello from Haskell!"
  putStrLn msg
  output currPlugin 0 (JSON $ Count 999)

main = do
  setLogFile "stdout" LogError
  let m = manifest [wasmFile "wasm/code-functions.wasm"]
  f <- hostFunction "hello_world" [I64] [I64] hello "Hello, again"
  plugin <- unwrap <$> newPlugin m [f] True
  id <- pluginID plugin
  print id
  JSON res <- (unwrap <$> call plugin "count_vowels" "this is a test" :: IO (JSON Count))
  print res
