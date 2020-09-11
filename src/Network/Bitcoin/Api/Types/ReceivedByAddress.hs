{-# LANGUAGE OverloadedStrings #-}
module Network.Bitcoin.Api.Types.ReceivedByAddress(
    ReceivedByAddress(..)
  ) where

import           Control.Monad       (mzero)

import qualified Data.Base58String   as B58S
import           Data.Word           (Word32, Word64)

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Bitcoin.Types  as BT

import Data.Text (Text, unpack)

data ReceivedByAddress = ReceivedByAddress {
  rbaInvolvesWhatchonly :: !Bool
, rbaAddress            :: !Text
, rbaAmount             :: !BT.Btc
, rbaConfirmations      :: !Int
, rbaLabel              :: !Text
, rbaTxids              :: ![BT.TransactionId]
}

instance FromJSON ReceivedByAddress where
  parseJSON = withObject "ReceivedByAddress" $ \o -> ReceivedByAddress
    <$> o .:? "involvesWatchonly" .!=  False
    <*> o .: "address"
    <*> o .: "amount"
    <*> o .: "confirmations"
    <*> o .: "label"
    <*> o .: "txids"
