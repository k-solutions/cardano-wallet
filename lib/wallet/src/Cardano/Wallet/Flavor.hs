{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Flavor
    ( WalletFlavorS (..)
    , WalletFlavor (..)
    , KeyOf
    , TestState (..)
    , KeyFlavorS (..)
    , keyFlavorFromState
    , keyOfWallet
    , NetworkOf
    , StateWithKey
    , FlavorOf
    , WalletFlavors (..)
    , Excluding
    , shelleyOrShared
    , notByronKey
    , IncludingStates
    , KeyFlavor (..)
    , CredFromOf
    )
where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth (CredFromKeyK, CredFromScriptK) )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndAnyState, RndState (..) )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqAnyState, SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState (..) )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant )
import Cardano.Wallet.TypeLevel
    ( Excluding, Including )
import Data.Kind
    ( Type )
import GHC.Generics
    ( Generic )

-- | A singleton type to capture the flavor of a state.
data WalletFlavorS s where
    ShelleyWallet :: WalletFlavorS (SeqState n ShelleyKey)
    IcarusWallet :: WalletFlavorS (SeqState n IcarusKey)
    ByronWallet :: WalletFlavorS (RndState n)
    SharedWallet :: WalletFlavorS (SharedState n SharedKey)
    BenchByronWallet :: WalletFlavorS (RndAnyState n p)
    BenchShelleyWallet :: WalletFlavorS (SeqAnyState n ShelleyKey p)
    TestStateS :: WalletFlavorS (TestState s ShelleyKey)

data WalletFlavors
    = ShelleyF
    | IcarusF
    | ByronF
    | SharedF
    | BenchByronF
    | BenchShelleyF
    | TestStateF

type family FlavorOf s where
    FlavorOf (SeqState n ShelleyKey) = 'ShelleyF
    FlavorOf (SeqState n IcarusKey) = 'IcarusF
    FlavorOf (RndState n) = 'ByronF
    FlavorOf (SharedState n SharedKey) = 'SharedF
    FlavorOf (RndAnyState n p) = 'BenchByronF
    FlavorOf (SeqAnyState n ShelleyKey p) = 'BenchShelleyF
    FlavorOf (TestState s ShelleyKey) = 'TestStateF


type AllFlavors =
    '[ 'ShelleyF
     , 'IcarusF
     , 'ByronF
     , 'SharedF
     , 'BenchByronF
     , 'BenchShelleyF
     , 'TestStateF
     ]

type IncludingStates ss s = Including AllFlavors ss s

-- | A function to reify the flavor of a state.
class WalletFlavor s where
    walletFlavor :: WalletFlavorS s

instance WalletFlavor (SeqState n IcarusKey) where
    walletFlavor = IcarusWallet

instance WalletFlavor (SeqState n ShelleyKey) where
    walletFlavor = ShelleyWallet

instance WalletFlavor (RndState n) where
    walletFlavor = ByronWallet

instance WalletFlavor (SeqAnyState n ShelleyKey p) where
    walletFlavor = BenchShelleyWallet

instance WalletFlavor (RndAnyState n p) where
    walletFlavor = BenchByronWallet

instance WalletFlavor (SharedState n SharedKey) where
    walletFlavor = SharedWallet

instance WalletFlavor (TestState n ShelleyKey) where
    walletFlavor = TestStateS

-- | A type for states that will be used in tests.
newtype TestState s (k :: (Depth -> Type -> Type)) = TestState s
    deriving (Generic, Show, Eq)

-- | A type family to get the key type from a state.
type family KeyOf (s :: Type) :: (Depth -> Type -> Type) where
    KeyOf (SeqState n k) = k
    KeyOf (RndState n) = ByronKey
    KeyOf (SharedState n k) = k
    KeyOf (SeqAnyState n k p) = k
    KeyOf (RndAnyState n p) = ByronKey
    KeyOf (TestState s k) = k

-- | A singleton type to capture the flavor of a key.
data KeyFlavorS a where
    ByronKeyS :: KeyFlavorS ByronKey
    IcarusKeyS :: KeyFlavorS IcarusKey
    ShelleyKeyS :: KeyFlavorS ShelleyKey
    SharedKeyS :: KeyFlavorS SharedKey


class KeyFlavor a where
    keyFlavor :: KeyFlavorS a

instance KeyFlavor ByronKey where
    keyFlavor = ByronKeyS

instance KeyFlavor IcarusKey where
    keyFlavor = IcarusKeyS

instance KeyFlavor ShelleyKey where
    keyFlavor = ShelleyKeyS

instance KeyFlavor SharedKey where
    keyFlavor = SharedKeyS


-- | Map a wallet flavor to a key flavor.
keyOfWallet :: WalletFlavorS s -> KeyFlavorS (KeyOf s)
keyOfWallet ShelleyWallet = ShelleyKeyS
keyOfWallet IcarusWallet = IcarusKeyS
keyOfWallet ByronWallet = ByronKeyS
keyOfWallet SharedWallet = SharedKeyS
keyOfWallet BenchByronWallet = ByronKeyS
keyOfWallet BenchShelleyWallet = ShelleyKeyS
keyOfWallet TestStateS = ShelleyKeyS

-- | A function to reify the flavor of a key from a state type.
--
-- use with
-- > keyFlavorFromState @s
keyFlavorFromState :: forall s. WalletFlavor s => KeyFlavorS (KeyOf s)
keyFlavorFromState = keyOfWallet (walletFlavor @s)

type family NetworkOf (s :: Type) :: NetworkDiscriminant where
    NetworkOf (SeqState n k) = n
    NetworkOf (RndState n) = n
    NetworkOf (SharedState n k) = n
    NetworkOf (SeqAnyState n k p) = n
    NetworkOf (RndAnyState n p) = n

-- | Constraints for a state with a specific key.
type StateWithKey s k = (WalletFlavor s, KeyOf s ~ k)

notByronKey :: KeyFlavorS k
    -> (Excluding '[ByronKey] k  => KeyFlavorS k -> x)
    -> Maybe x
notByronKey x h = case x of
    ByronKeyS -> Nothing
    ShelleyKeyS -> Just $ h ShelleyKeyS
    IcarusKeyS -> Just $ h IcarusKeyS
    SharedKeyS -> Just $ h SharedKeyS

-- | Helper lemma to specialize on a subset of wallet flavors.
shelleyOrShared
    :: WalletFlavorS s
    -> x
    -> (IncludingStates '[ 'IcarusF, 'ShelleyF, 'SharedF] (FlavorOf s)
            => WalletFlavorS s -> x)
    -> x
shelleyOrShared x r h = case x of
    ShelleyWallet -> h ShelleyWallet
    SharedWallet -> h SharedWallet
    IcarusWallet -> h IcarusWallet
    _ -> r

type family CredFromOf s where
    CredFromOf (SharedState n key) = 'CredFromScriptK
    CredFromOf (SeqState n key) = 'CredFromKeyK
    CredFromOf (RndState n) = 'CredFromKeyK
    CredFromOf (TestState n key) = 'CredFromKeyK
    CredFromOf (RndAnyState n p) = 'CredFromKeyK
    CredFromOf (SeqAnyState n key p) = 'CredFromKeyK
