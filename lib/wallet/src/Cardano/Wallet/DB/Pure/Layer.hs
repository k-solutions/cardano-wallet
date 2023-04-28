{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Dummy implementation of the database-layer, using 'MVar'. This may be good
-- for testing to compare with an implementation on a real data store, or to use
-- when compiling the wallet for targets which don't have SQLite.

module Cardano.Wallet.DB.Pure.Layer
    ( newDBFresh
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBFresh (..)
    , DBLayer (..)
    , ErrWalletAlreadyInitialized (ErrWalletAlreadyInitialized)
    , ErrWalletNotInitialized (..)
    )
import Cardano.Wallet.DB.Pure.Implementation
    ( Database
    , Err (..)
    , ModelOp
    , mCheckWallet
    , mGetWalletId
    , mInitializeWallet
    , mIsStakeKeyRegistered
    , mListCheckpoints
    , mPutCheckpoint
    , mPutDelegationCertificate
    , mPutDelegationRewardBalance
    , mPutPrivateKey
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadGenesisParameters
    , mReadPrivateKey
    , mReadTxHistory
    , mReadWalletMeta
    , mRollbackTo
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..), WalletId, wholeRange )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TransactionInfo (..) )
import Control.Monad
    ( join, unless, when )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO (..) )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Maybe
    ( fromMaybe )
import UnliftIO.Exception
    ( Exception, throwIO )
import UnliftIO.MVar
    ( MVar, isEmptyMVar, modifyMVar, newEmptyMVar, newMVar, putMVar, withMVar )

-- | Instantiate a new in-memory "database" layer that simply stores data in
-- a local MVar. Data vanishes if the software is shut down.
newDBFresh
    :: forall m s k.
       ( MonadUnliftIO m
       , MonadFail m )
    => TimeInterpreter Identity
    -> WalletId
    -> m (DBFresh m s k)
newDBFresh timeInterpreter wid = do
    lock <- newMVar ()
    db <- newEmptyMVar

    let
      getWalletId' = ExceptT
            $ alterDB errWalletNotInitialized db mGetWalletId
      dbf = DBFresh
            {
              bootDBLayer = \sp -> do
                t <- isEmptyMVar db
                unless t $ throwE ErrWalletAlreadyInitialized
                liftIO
                    $ putMVar db
                    $ mInitializeWallet wid sp
                pure dbl
            , loadDBLayer = error "loadDBLayer: not implemented"
              }
      dbl = DBLayer


        {-----------------------------------------------------------------------
                                      Wallets
        -----------------------------------------------------------------------}

        { walletId_ = wid

        {-----------------------------------------------------------------------
                                    Checkpoints
        -----------------------------------------------------------------------}
        , walletsDB = error "MVar.walletsDB: not implemented"

        , putCheckpoint = \cp -> ExceptT $
            alterDB errWalletNotInitialized db (mCheckWallet) >>= \case
                Left err -> pure $ Left err
                Right _ -> do
                    alterDB errWalletNotInitialized db $
                        mPutCheckpoint cp

        , readCheckpoint = join <$> readDBMaybe db mReadCheckpoint

        , listCheckpoints = fromMaybe [] <$> readDBMaybe db mListCheckpoints

        , rollbackTo = ExceptT
            . alterDB errWalletNotInitialized db
            . mRollbackTo

        , prune = \_ _ -> error "MVar.prune: not implemented"

        {-----------------------------------------------------------------------
                                   Wallet Metadata
        -----------------------------------------------------------------------}

        , putWalletMeta = ExceptT
            .  alterDB errWalletNotInitialized db
            .  mPutWalletMeta

        , readWalletMeta = fmap join
            $ readDBMaybe db
            $ mReadWalletMeta timeInterpreter

        , putDelegationCertificate = \cert sl -> ExceptT $
            alterDB errWalletNotInitialized db $
            mPutDelegationCertificate cert sl

        , isStakeKeyRegistered =
            ExceptT . alterDB errWalletNotInitialized db $ mIsStakeKeyRegistered

        {-----------------------------------------------------------------------
                                     Tx History
        -----------------------------------------------------------------------}

        , putTxHistory = ExceptT
            . alterDB errWalletNotInitialized db
            . mPutTxHistory

        , readTransactions = \minWithdrawal order range mstatus _mlimit ->
            fmap (fromMaybe []) $
            readDBMaybe db $
                mReadTxHistory
                    timeInterpreter
                    minWithdrawal
                    order
                    range
                    mstatus

        -- TODO: shift implementation to mGetTx
        , getTx = \tid -> do
            wid' <- getWalletId'
            when ( wid /= wid') $ throwE ErrWalletNotInitialized
            ExceptT $ do
                alterDB errWalletNotInitialized db (mCheckWallet) >>= \case
                    Left err -> pure $ Left err
                    Right _ -> do
                        txInfos <- fmap (fromMaybe [])
                            $ readDBMaybe db
                            $ mReadTxHistory
                                timeInterpreter
                                Nothing
                                Descending
                                wholeRange
                                Nothing
                        let txPresent (TransactionInfo{..}) = txInfoId == tid
                        case filter txPresent txInfos of
                            [] -> pure $ Right Nothing
                            t:_ -> pure $ Right $ Just t

        {-----------------------------------------------------------------------
                                       Keystore
        -----------------------------------------------------------------------}

        , putPrivateKey = ExceptT
            . alterDB errWalletNotInitialized db
            . mPutPrivateKey

        , readPrivateKey = join <$> readDBMaybe db mReadPrivateKey

        {-----------------------------------------------------------------------
                                       Pending Tx
        -----------------------------------------------------------------------}

        , addTxSubmission =
            error "addTxSubmission not tested in State Machine tests"

        , readLocalTxSubmissionPending =
            error "readLocalTxSubmissionPending not tested in State Machine tests"

        , resubmitTx =
            error "resubmitTx not tested in State Machine tests"

        , rollForwardTxSubmissions =
            error "rollForwardTxSubmissions not tested in State Machine tests"

        , removePendingOrExpiredTx = error
            "removePendingOrExpiredTx not implemented in State Machine tests"

        {-----------------------------------------------------------------------
                                 Protocol Parameters
        -----------------------------------------------------------------------}

        , readGenesisParameters = join <$> readDBMaybe db mReadGenesisParameters

        {-----------------------------------------------------------------------
                                 Delegation Rewards
        -----------------------------------------------------------------------}

        , putDelegationRewardBalance = ExceptT
            . alterDB errWalletNotInitialized db
            . mPutDelegationRewardBalance

        , readDelegationRewardBalance = fromMaybe (Coin 0)
            <$> readDBMaybe db mReadDelegationRewardBalance

        {-----------------------------------------------------------------------
                                      Execution
        -----------------------------------------------------------------------}

        , atomically = \action -> withMVar lock $ \() -> action
        }
    pure dbf

-- | Read the database, but return 'Nothing' if the operation fails.
readDBMaybe :: MonadUnliftIO f
    => MVar (Database WalletId s xprv)
    -> ModelOp WalletId s xprv a
    -> f (Maybe a)
readDBMaybe db = fmap (either (const Nothing) Just) . readDB db

-- | Apply an operation to the model database, then update the mutable variable.
-- Failures are converted to 'Err' using the provided function.
-- Failures that cannot be converted are rethrown as 'MVarDBError'.
alterDB
    :: MonadUnliftIO m
    => (Err -> Maybe err)
    -- ^ Error type converter
    -> MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either err a)
alterDB convertErr db op = modifyMVar db (bubble . op)
  where
    bubble (Left e, !db') = case convertErr e of
        Just e' -> pure (db', Left e')
        Nothing -> throwIO $ MVarDBError e
    bubble (Right a, !db') = pure (db', Right a)

-- | Run a query operation on the model database.
readDB
    :: MonadUnliftIO m
    => MVar (Database WalletId s xprv)
    -- ^ The database variable
    -> ModelOp WalletId s xprv a
    -- ^ Operation to run on the database
    -> m (Either Err a)
readDB = alterDB Just -- >>= either (throwIO . MVarDBError) pure

errWalletNotInitialized :: Err -> Maybe ErrWalletNotInitialized
errWalletNotInitialized WalletNotInitialized = Just ErrWalletNotInitialized
errWalletNotInitialized _ = Nothing

-- | Error which happens when model returns an unexpected value.
newtype MVarDBError = MVarDBError Err
    deriving (Show)

instance Exception MVarDBError
