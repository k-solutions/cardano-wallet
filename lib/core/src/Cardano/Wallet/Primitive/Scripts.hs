{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of wallet's verification key discovery
-- in a script.

module Cardano.Wallet.Primitive.Scripts
    ( isShared
    , retrieveAllVerKeyHashes
    , toKeyHash
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubPublicKey )
import Cardano.Address.Script
    ( KeyHash (..), Script (..), ScriptHash (..), toScriptHash )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Role (..)
    , SoftDerivation (..)
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..), accountPubKey )
import Crypto.Hash.Utils
    ( blake2b224 )

import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T

instance Ord ScriptHash where
    compare (ScriptHash sh1) (ScriptHash sh2) =
        compare (T.decodeUtf8 $ hex sh1) (T.decodeUtf8 $ hex sh2)

isShared
    :: (k ~ ShelleyKey, SoftDerivation k)
    => Script
    -> SeqState n k
    -> ([k 'ScriptK XPub], SeqState n k)
isShared script s@(SeqState !s1 !s2 !ixs !rpk !prefix !scripts) =
    let verKeysInScript = retrieveAllVerKeyHashes script
        accXPub = accountPubKey s2
        toVerKey ix = deriveAddressPublicKey accXPub MultisigScript ix
        minIndex = getIndex @'Soft minBound
        scriptAddressGap = 10
        ourVerKeys =
            map (\ix -> toVerKey (toEnum (fromInteger $ toInteger $ minIndex + ix)))
            [0 .. scriptAddressGap]
        ourVerKeyHashesInScript =
            filter (\keyH -> toKeyHash keyH `elem` verKeysInScript)
            ourVerKeys
        toScriptXPub (ShelleyKey k) = ShelleyKey k
        scriptXPubs = map toScriptXPub ourVerKeyHashesInScript
    in if null ourVerKeyHashesInScript then
        ([], s)
       else
        ( scriptXPubs
        , SeqState s1 s2 ixs rpk prefix (Map.insert (toScriptHash script) scriptXPubs scripts))
  where

retrieveAllVerKeyHashes :: Script -> [KeyHash]
retrieveAllVerKeyHashes = extractVerKey []
  where
      extractVerKey acc (RequireSignatureOf verKey) = verKey : acc
      extractVerKey acc (RequireAllOf xs) = foldr (flip extractVerKey) acc xs
      extractVerKey acc (RequireAnyOf xs) = foldr (flip extractVerKey) acc xs
      extractVerKey acc (RequireSomeOf _ xs) = foldr (flip extractVerKey) acc xs

toKeyHash :: ShelleyKey depth XPub -> KeyHash
toKeyHash = KeyHash . blake2b224 . xpubPublicKey . getKey
