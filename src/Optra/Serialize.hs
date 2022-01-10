{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Optra.Serialize
  (
    serializeOp,
    serializeOps,
    deserializeOp,
    deserializeOps
  ) where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text.Lazy as DT
import qualified Data.Text.Lazy.Encoding as DTE
import qualified Data.ByteString.Lazy as DB
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import Optra.Operation as OP


data OpInfo = OpInfo
    {
      opType   :: DT.Text
    , paramInt :: Int
    , paramStr :: DT.Text
    } deriving (Generic, Show)


instance FromJSON OpInfo where
    parseJSON = withObject "OpInfo" $ \v ->
      OpInfo
        <$> v .: "opType"
        <*> v .: "paramInt"
        <*> v .: "paramStr"


instance ToJSON OpInfo where
    toEncoding = genericToEncoding defaultOptions


-- | Convert OpInfo to OP.Operation
fromOpInfo :: OpInfo -> OP.Operation
fromOpInfo (OpInfo opType paramInt paramStr)
   | opType == (DT.pack "retain") = OP.Retain paramInt
   | opType == (DT.pack "delete") = OP.Delete paramInt
   | opType == (DT.pack "insert") = OP.Insert (DT.unpack paramStr)
   | otherwise                    = OP.NoOp


-- | Convert OP.Operation to OpInfo
toOpInfo :: OP.Operation -> OpInfo
toOpInfo (OP.Retain n) = OpInfo (DT.pack "retain") n (DT.pack "")
toOpInfo (OP.Delete n) = OpInfo (DT.pack "delete") n (DT.pack "")
toOpInfo (OP.Insert s) = OpInfo (DT.pack "insert") 0 (DT.pack s)
toOpInfo OP.NoOp       = OpInfo (DT.pack "noop") 0 (DT.pack "")


data OpSeqInfo = OpSeqInfo
    {
      baseLen   :: Int
    , targetLen :: Int
    , ops       :: [DT.Text]
    }  deriving (Generic, Show)


instance FromJSON OpSeqInfo where
    parseJSON = withObject "OpSeqInfo" $ \v ->
      OpSeqInfo
        <$> v .: "baseLen"
        <*> v .: "targetLen"
        <*> v .: "ops"


instance ToJSON OpSeqInfo where
    toEncoding = genericToEncoding defaultOptions


-- | Convert OpSeqInfo to OP.OperationSeq
fromOpSeqInfo :: OpSeqInfo -> Maybe OP.OperationSeq
fromOpSeqInfo (OpSeqInfo blen tlen ops) = do
    decodedOps <- mapM decode (map DTE.encodeUtf8 ops) :: Maybe [OpInfo]
    let ops' = DS.fromList $ map fromOpInfo decodedOps
    return $ OP.OperationSeq blen tlen ops' 


-- | Convert OP.OperationSeq to OpSeqInfo
toOpSeqInfo :: OP.OperationSeq -> Maybe OpSeqInfo
toOpSeqInfo (OP.OperationSeq blen tlen ops) = do
    let opInfos    = map toOpInfo $ DF.toList ops
        encodedOps = map (DTE.decodeUtf8 . encode) opInfos
    return $ OpSeqInfo blen tlen encodedOps


-- | Serialize OP.Operation to JSON
serializeOp :: OP.Operation -> DB.ByteString
serializeOp op = encode $ toOpInfo op


-- | Serialize OP.OperationSeq to JSON
serializeOps :: OP.OperationSeq -> DB.ByteString
serializeOps ops = encode $ toOpSeqInfo ops 


-- | Deserialize to OP.Operation from JSON
deserializeOp :: DB.ByteString -> Maybe OP.Operation
deserializeOp op = do
    decoded <- decode op :: Maybe OpInfo
    return $ fromOpInfo decoded


-- | Deserialize to OP.OperationSeq from JSON
deserializeOps :: DB.ByteString -> Maybe OP.OperationSeq
deserializeOps ops = do
    decoded <- decode ops :: Maybe OpSeqInfo
    opSeq <- fromOpSeqInfo decoded
    return opSeq

