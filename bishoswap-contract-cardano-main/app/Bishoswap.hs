{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Bashoswap where

import           Control.Monad                       (forM_, when)
import           Data.Aeson                          (FromJSON, ToJSON)
import qualified Data.Semigroup                      as Semigroup
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Ledger
import           Ledger.Constraints
import           Ledger.Value                        as Value
import           Plutus.Contract
import qualified Plutus.Contracts.Currency           as Currency
import qualified Plutus.Contracts.Uniswap            as Bashoswap
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

data BashoswapContracts =
      Init
    | BashoswapStart
    | BashoswapUser Bashoswap.Uniswap
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty BashoswapContracts where
    pretty = viaShow

instance Builtin.HasDefinitions BashoswapContracts where
    getDefinitions = [Init, BashoswapStart]
    getSchema = \case
        BashoswapUser _ -> Builtin.endpointsToSchemas @Bashoswap.UniswapUserSchema
        BashoswapStart  -> Builtin.endpointsToSchemas @Bashoswap.UniswapOwnerSchema
        Init          -> Builtin.endpointsToSchemas @Empty
    getContract = \case
        BashoswapUser us -> Builtin.SomeBuiltin $ Bashoswap.userEndpoints us
        BashoswapStart   -> Builtin.SomeBuiltin Bashoswap.ownerEndpoint
        Init           -> Builtin.SomeBuiltin initContract

initContract :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <- Currency.mintContract ownPK [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]

cidFile :: Wallet -> FilePath
cidFile w = "W" ++ show (getWallet w) ++ ".cid"
