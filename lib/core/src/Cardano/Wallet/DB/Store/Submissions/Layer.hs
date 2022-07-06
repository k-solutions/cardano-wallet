{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
module Cardano.Wallet.DB.Store.Submissions.Layer where

import Prelude

import Cardano.Wallet.DB.Sqlite.Migration
    ()
import Cardano.Wallet.DB.Store.Wallets.Model
    ( TxWalletsHistory )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( PersistField (toPersistValue), SqlPersistT, rawExecute )
import Cardano.Wallet.DB.Sqlite.Types (TxId)
import Cardano.Wallet.DB.Store.Meta.Model (TxMetaHistory(..))
import Cardano.Wallet.DB.Sqlite.Schema (TxMeta(txMetaBlockHeight))

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Map.Strict as Map
import Cardano.Wallet.DB.Store.Submissions.Model (TxLocalSubmissionHistory, DeltaTxLocalSubmission (..))
import Data.Maybe (fromMaybe)

-- | Remove transactions from the local submission pool once they can no longer
-- be rolled back.
pruneLocalTxSubmission
    :: W.WalletId
    -> Quantity "block" Word32
    -> W.BlockHeader
    -> SqlPersistT IO ()
pruneLocalTxSubmission wid (Quantity epochStability) tip =
    rawExecute query params
  where
    query =
        "DELETE FROM local_tx_submission " <>
        "WHERE wallet_id=? AND tx_id IN " <>
        "( SELECT tx_id FROM tx_meta WHERE tx_meta.block_height < ? )"
    params = [toPersistValue wid, toPersistValue stableHeight]
    stableHeight = getQuantity (tip ^. #blockHeight) - epochStability

-- SELECT tx_id FROM tx_meta WHERE tx_meta.block_height < ?
selectOldTxIds
    :: W.WalletId -- wallet
    -> Word32 -- height
    -> TxWalletsHistory -- transaction
    -> Maybe [TxId]
selectOldTxIds wid stableHeight (_, metasAndSubmissions) = do
    (TxMetaHistory metas, _) <- Map.lookup wid metasAndSubmissions
    pure $ Map.keys $ Map.filter ((< stableHeight) . txMetaBlockHeight ) metas

-- | Remove transactions from the local submission pool once they can no longer
-- be rolled back.
pruneLocalTxSubmissionNew
    :: W.WalletId
    -> Quantity "block" Word32
    -> W.BlockHeader
    -> TxWalletsHistory
    -> DeltaTxLocalSubmission
pruneLocalTxSubmissionNew wid (Quantity epochStability) tip txs =
    Prune $ fromMaybe [] $ selectOldTxIds wid stableHeight $ txs
  where
    stableHeight = getQuantity (tip ^. #blockHeight) - epochStability
