{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Types.TxInfo where

import           Control.Monad       (mzero)

import qualified Data.Base58String   as B58S
import           Data.Word           (Word32, Word64)

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Bitcoin.Types  as BT

import Data.Text (Text, unpack)

data TxInfo = TxInfo {
   txid        :: BT.TransactionId
  ,vins        :: [Vin]
  ,vouts       :: [Vout]
  ,confs       :: Integer
  ,blockhash   :: BT.BlockHash
  ,timestamp   :: Word64
} deriving (Eq, Show)


instance FromJSON TxInfo where
  parseJSON (Object o) = TxInfo
         <$> o .: "txid"
         <*> o .: "vin"
         <*> o .: "vout"
         <*> o .: "confirmations"
         <*> o .: "blockhash"
         <*> o .: "time"
  parseJSON _          = mzero

data RawTxInfo = RawTxInfo {
    rawTxInfoHex           :: !Text
  , rawTxInfoTxid          :: !BT.TransactionId
  , rawTxInfoHash          :: !BT.TransactionId
  , rawTxInfoSize          :: !Int
  , rawTxInfoVSize         :: !Int
  , rawTxInfoVersion       :: !Int
  , rawTxInfoLocktime      :: !Word64
  , rawTxInfoBlockHash     :: !(Maybe BT.BlockHash)
  , rawTxInfoConfirmations :: !Integer
  , rawTxInfoTime          :: !Word64
  , rawTxInfoBlockTime     :: !Word64
  -- , rawTxInfoVin           :: ![RawVin]
  -- , rawTxInfoVout          :: ![RawVout]
} deriving (Eq, Show)

instance FromJSON RawTxInfo where
  parseJSON (Object o) = RawTxInfo
         <$> o .: "hex"
         <*> o .: "txid"
         <*> o .: "hash"
         <*> o .: "size"
         <*> o .: "vsize"
         <*> o .: "version"
         <*> o .: "locktime"
         <*> o .:? "blockhash"
         <*> o .: "confirmations"
         <*> o .: "time"
         <*> o .: "blocktime"
         -- <*> o .: "vin"
         -- <*> o .: "vout"
  parseJSON _          = mzero

data Vout = Vout {
   amount      :: BT.Btc
  ,index       :: Word32
  ,addresses   :: [B58S.Base58String]
} deriving (Eq, Show)


instance FromJSON Vout where
  parseJSON (Object o) =
    Vout
      <$> o .:  "value"
      <*> o .:  "n"
      <*> ( (o .:  "scriptPubKey") >>= getOptAddr )
    where
      getOptAddr o' = o' .:? "addresses" .!= []
  parseJSON _          = mzero

data Vin =
    Vin {
        ref_txid        :: BT.TransactionId
      , ref_index       :: Word32
    } |
    CoinbaseVin -- A Coinbase transaction is the first transaction in a block
        deriving (Eq, Show)

instance FromJSON Vin where
    parseJSON (Object o) = o .:! "txid" >>=
        \maybeTxId -> case maybeTxId of
            Nothing -> return CoinbaseVin
            Just txid -> Vin txid <$> o .: "vout"
    parseJSON _          = mzero
