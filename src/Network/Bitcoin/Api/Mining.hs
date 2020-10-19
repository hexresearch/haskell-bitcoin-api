{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Mining where

import           Data.Aeson
import           Data.Maybe

import qualified Data.Bitcoin.Block             as Btc
import qualified Data.Bitcoin.Types             as BT

import qualified Network.Bitcoin.Api.Internal   as I
import qualified Network.Bitcoin.Api.Types      as T
import qualified Network.Bitcoin.Api.Blockchain as Blockchain

-- | Generate a certain amount of new blocks. Available in 'regtest' mode only.
generate :: T.Client       -- ^ Our client context
         -> Integer        -- ^ Amount of blocks to generate
         -> IO [Btc.Block] -- ^ The generated blocks
generate client blocks =
  let configuration = [toJSON blocks]
  in do
    hashes <- I.call client "generate" configuration
    catMaybes <$> mapM (Blockchain.getBlock client) hashes

-- | Generate a certain amount of new blocks and send coinbase to given address.
-- Available in 'regtest' mode only.
generateToAddress :: T.Client -> Integer -> BT.Address -> IO [Btc.Block]
generateToAddress client blocks addr = do
  let configuration = [toJSON blocks, toJSON addr]
  hashes <- I.call client "generatetoaddress" configuration
  catMaybes <$> mapM (Blockchain.getBlock client) hashes
