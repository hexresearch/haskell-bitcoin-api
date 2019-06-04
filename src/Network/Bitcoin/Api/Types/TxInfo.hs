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

data GetTxInfo = GetTxInfo {
    getTxInfoAmount        :: !BT.Btc
  , getTxInfoFee           :: !BT.Btc
  , getTxInfoConfirmations :: !Integer
  , getTxInfoBlockHash     :: !BT.BlockHash
  , getTxInfoBlockIndex    :: !Int
  , getTxInfoBlockTime     :: !Word64
  , getTxInfoTxid          :: !BT.TransactionId
  , getTxInfoTime          :: !Word64
  , getTxInfoReceived      :: !Word64
  , getTxInfoReplaceable   :: !Replacable
  , getTxInfoHex           :: !Text
  , getTxInfoDetails       :: ![TxInfoDetails]
} deriving (Eq, Show)

instance FromJSON GetTxInfo where
  parseJSON (Object o) = GetTxInfo
         <$> o .: "amount"
         <*> o .: "fee"
         <*> o .: "confirmations"
         <*> o .: "blockhash"
         <*> o .: "blockindex"
         <*> o .: "blocktime"
         <*> o .: "txid"
         <*> o .: "time"
         <*> o .: "timereceived"
         <*> o .: "bip125-replaceable"
         <*> o .: "hex"
         <*> o .: "details"
  parseJSON _          = mzero

data TxInfoDetails = TxInfoDetails {
    detailsAddress    :: !Text
  , detailsCategory   :: !TxCategory
  , detailsAmount     :: !BT.Btc
  , detailsLabel      :: !Text
  , detailsVout       :: !Int
  , detailsFee        :: !BT.Btc
  , detailsAbandoned  :: !Bool
} deriving (Eq, Show)

instance FromJSON TxInfoDetails where
  parseJSON (Object o) = TxInfoDetails
         <$> o .: "address"
         <*> o .: "category"
         <*> o .: "amount"
         <*> o .: "label"
         <*> o .: "vout"
         <*> o .: "fee"
         <*> o .: "abandoned"
  parseJSON _          = mzero

data TxCategory = TxSend | TxReceive
  deriving (Eq, Show)

instance FromJSON TxCategory where
  parseJSON (String s) = case s of
    "send" -> pure TxSend
    "receive" -> pure TxReceive
    v -> fail $ "Unknown value for TxInfoDetails " ++ unpack v
  parseJSON _          = mzero

data Replacable = RepleacableYes | ReplacableNot | ReplacableUnknown
  deriving (Eq, Show)

instance FromJSON Replacable where
  parseJSON (String s) = case s of
    "yes" -> pure RepleacableYes
    "not" -> pure ReplacableNot
    "unknown" -> pure ReplacableUnknown
    v -> fail $ "Unknown value for Replacable " ++ unpack v
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
