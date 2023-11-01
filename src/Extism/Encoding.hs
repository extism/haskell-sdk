{-# LANGUAGE FlexibleInstances #-}

-- |
-- Extism.Encoding handles how values are encoded to be copied in and out of Wasm linear memory
module Extism.Encoding
  ( fromByteString,
    toByteString,
    Error (..),
    Result (..),
    ToBytes (..),
    FromBytes (..),
    Encoding (..),
    JSONValue (..),
  )
where

import Data.Binary.Get (getDoublele, getFloatle, getInt32le, getInt64le, getWord32le, getWord64le, runGetOrFail)
import Data.Binary.Put (putDoublele, putFloatle, putInt32le, putInt64le, putWord32le, putWord64le, runPut)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, unsafePackLenAddress, w2c)
import Data.Int
import Data.Word
import qualified Extism.JSON (JSON (..))
import qualified Text.JSON (JSValue, Result (..), decode, encode, showJSON, toJSObject)
import qualified Text.JSON.Generic (Data, decodeJSON, encodeJSON, fromJSON, toJSON)

-- | Helper function to convert a 'String' to a 'ByteString'
toByteString :: String -> B.ByteString
toByteString x = B.pack (map c2w x)

-- | Helper function to convert a 'ByteString' to a 'String'
fromByteString :: B.ByteString -> String
fromByteString bs = map w2c $ B.unpack bs

-- | Extism error
newtype Error = ExtismError String deriving (Show, Eq)

-- | Result type
type Result a = Either Error a

-- Used to convert a value into linear memory
class ToBytes a where
  toBytes :: a -> B.ByteString

-- Used to read a value from linear memory
class FromBytes a where
  fromBytes :: B.ByteString -> Result a

-- Encoding is used to indicate a type implements both `ToBytes` and `FromBytes`
class (ToBytes a, FromBytes a) => Encoding a

instance ToBytes () where
  toBytes () = toByteString ""

instance FromBytes () where
  fromBytes _ = Right ()

instance ToBytes B.ByteString where
  toBytes x = x

instance FromBytes B.ByteString where
  fromBytes = Right

instance ToBytes [Char] where
  toBytes = toByteString

instance FromBytes [Char] where
  fromBytes bs =
    Right $ fromByteString bs

instance ToBytes Int32 where
  toBytes i = B.toStrict (runPut (putInt32le i))

instance FromBytes Int32 where
  fromBytes bs =
    case runGetOrFail getInt32le (B.fromStrict bs) of
      Left (_, _, e) -> Left (ExtismError e)
      Right (_, _, x) -> Right x

instance ToBytes Int64 where
  toBytes i = B.toStrict (runPut (putInt64le i))

instance FromBytes Int64 where
  fromBytes bs =
    case runGetOrFail getInt64le (B.fromStrict bs) of
      Left (_, _, e) -> Left (ExtismError e)
      Right (_, _, x) -> Right x

instance ToBytes Word32 where
  toBytes i = B.toStrict (runPut (putWord32le i))

instance FromBytes Word32 where
  fromBytes bs =
    case runGetOrFail getWord32le (B.fromStrict bs) of
      Left (_, _, e) -> Left (ExtismError e)
      Right (_, _, x) -> Right x

instance ToBytes Word64 where
  toBytes i = B.toStrict (runPut (putWord64le i))

instance FromBytes Word64 where
  fromBytes bs =
    case runGetOrFail getWord64le (B.fromStrict bs) of
      Left (_, _, e) -> Left (ExtismError e)
      Right (_, _, x) -> Right x

instance ToBytes Float where
  toBytes i = B.toStrict (runPut (putFloatle i))

instance FromBytes Float where
  fromBytes bs =
    case runGetOrFail getFloatle (B.fromStrict bs) of
      Left (_, _, e) -> Left (ExtismError e)
      Right (_, _, x) -> Right x

instance ToBytes Double where
  toBytes i = B.toStrict (runPut (putDoublele i))

instance FromBytes Double where
  fromBytes bs =
    case runGetOrFail getDoublele (B.fromStrict bs) of
      Left (_, _, e) -> Left (ExtismError e)
      Right (_, _, x) -> Right x

-- Wraps a `JSON` value for input/output
newtype JSONValue x = JSONValue x

instance (Text.JSON.Generic.Data a) => ToBytes (JSONValue a) where
  toBytes (JSONValue x) =
    toByteString $ Text.JSON.Generic.encodeJSON x

instance (Text.JSON.Generic.Data a) => FromBytes (JSONValue a) where
  fromBytes bs =
    let x = Text.JSON.decode (fromByteString bs)
     in case x of
          Text.JSON.Error e -> Left (ExtismError e)
          Text.JSON.Ok x ->
            case Text.JSON.Generic.fromJSON (x :: Text.JSON.JSValue) of
              Text.JSON.Error e -> Left (ExtismError e)
              Text.JSON.Ok x -> Right (JSONValue x)
