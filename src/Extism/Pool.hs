module Extism.Pool
  ( Pool,
    newPool,
    addPlugin,
    withPlugin',
    withPlugin,
  )
where

import Extism (Function, Manifest, Plugin, PluginInput, newPlugin, reset, unwrap)
import qualified Simpoole as P

data Item = Item Plugin

data Pool = Pool
  { poolItems :: [(String, P.Pool IO Plugin)],
    poolMax :: Int
  }

newPool n = Pool [] n

addPlugin :: (PluginInput a) => String -> a -> [Function] -> Bool -> Pool -> IO Pool
addPlugin k manifest functions wasi pool = do
  p <- P.newPool x reset settings
  pure $ pool {poolItems = (k, p) : poolItems pool}
  where
    x = fmap unwrap $ newPlugin manifest functions wasi
    settings = P.defaultSettings {P.settings_maxLiveLimit = Just (poolMax pool)}

withPlugin' :: Pool -> String -> (Plugin -> IO a) -> IO (Maybe a)
withPlugin' pool k f = do
  case x of
    Nothing -> pure Nothing
    Just x' ->
      fmap Just $ P.withResource x' f
  where
    x = lookup k (poolItems pool)

withPlugin :: Pool -> String -> (Plugin -> IO a) -> IO a
withPlugin pool k f = do
  x <- withPlugin' pool k f
  case x of
    Nothing -> error ("Key not found " ++ k)
    Just x -> pure x
