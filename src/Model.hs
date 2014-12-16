{-# LANGUAGE FlexibleInstances #-}

module Model where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time
import Prelude
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Applicative (pure)
import Fields
import qualified Data.HashMap.Lazy as HML
import Data.Aeson.Types (Parser)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- Custom data types
data CurrencyInput = CurrencyInput
    { currencyInputName :: Text
    , currencyInputUnit :: Text
    }

data WalletInput = WalletInput
    { walletInputName :: Text
    , walletInputAllowNegativeBalance :: Bool
    }

data Amount = Amount
    { currency :: Text
    , amount :: Double
    }
    deriving Show

instance FromJSON Amount where
    parseJSON (Object o) = Amount
        <$> o .: "currency"
        <*> o .: "amount"

data CurrencySelection = FirstFull
    deriving Show

instance ToJSON CurrencySelection where
    toJSON FirstFull = object
        [ "currency_selection" .= ("first_full" :: Text) ]

instance FromJSON CurrencySelection where
    parseJSON (String "first_full") = pure FirstFull
    parseJSON _ = mzero

-- Refactor this to be different types, because we don't want
-- fromWallet CancelTransaction to work. Would also make type checking
-- easier
data TransactionCommand =
      TransferValue
        { fromWallet        :: Text
        , toWallet          :: Text
        , amounts           :: [Amount]
        , transactionType   :: Text
        , currencySelection :: CurrencySelection
        }
    | ReserveValue
        { fromWallet        :: Text
        , toWallet          :: Text
        , amounts           :: [Amount]
        , transactionType   :: Text
        , currencySelection :: CurrencySelection
        }
    | CompleteTransaction
    | CancelTransaction
    deriving Show

data TransferMethod = Transfer | Reserve
    deriving (Show, Eq, Read)

-- Instances
instance ToJSON (Entity Currency) where
    toJSON (Entity cid c) = object
        [ "id"      .= (String $ toPathPiece cid)
        , "name"    .= currencyName c
        , "unit"    .= currencyUnit c
        ]

instance FromJSON CurrencyInput where
    parseJSON (Object o) = CurrencyInput
        <$> o .: "name"
        <*> o .: "unit"

    parseJSON _ = mzero

instance ToJSON (Entity TransactionType) where
    toJSON (Entity ttid tt) = object
        [ "id"          .= (String $ toPathPiece ttid)
        , "name"        .= transactionTypeName tt
        , "description" .= transactionTypeDescription tt
        ]

instance FromJSON TransactionType where
    parseJSON (Object o) = TransactionType
        <$> o .: "name"
        <*> o .: "description"

    parseJSON _ = mzero

instance ToJSON (Entity Wallet) where
    toJSON (Entity wid w) = object
        [ "id"                     .= (String $ toPathPiece wid)
        , "created"                .= walletCreated w
        , "name"                   .= walletName w
        , "allow_negative_balance" .= walletAllowNegativeBalance w
        ]

instance FromJSON WalletInput where
    parseJSON (Object o) = WalletInput
        <$> o .: "name"
        <*> o .: "allow_negative_balance"

    parseJSON _ = mzero

instance FromJSON TransactionCommand where
    parseJSON (Object o) =
        case HML.lookup "method" o of
            Just (String "reserve_value") -> ReserveValue
                <$> o  .: "from_wallet"
                <*> o  .: "to_wallet"
                <*> (o .: "amounts" >>= parseJSON)
                <*> o  .: "transaction_type"
                <*> (o .: "currency_selection" >>= parseJSON)
            Just (String "complete_transaction") -> pure CompleteTransaction
            Just (String "cancel_transaction") -> pure CancelTransaction
            Just (String "transfer_value") -> TransferValue
                <$> o  .: "from_wallet"
                <*> o  .: "to_wallet"
                <*> (o .: "amounts" >>= parseJSON)
                <*> o  .: "transaction_type"
                <*> (o .: "currency_selection" >>= parseJSON)

            _ -> mzero

instance ToJSON (Entity Transaction) where
    toJSON (Entity tid t) = object
        [ "id"      .= (String $ toPathPiece tid)
        , "created" .= transactionCreated t
        , "state_changed" .= transactionStateChanged t
        , "state" .= transactionState t
        , "from_wallet" .= transactionFromWallet t
        , "to_wallet" .= transactionToWallet t
        , "amount" .= transactionAmount t
        , "bonus_amount" .= transactionBonusAmount t
        , "currency" .= transactionCurrency t
        , "vat_percentage" .= transactionVatPercentage t
        , "transaction_type" .= transactionTransactionType t
        , "client_name" .= transactionClientName t
        ]

instance ToJSON (Entity WalletBalance) where
    toJSON (Entity bid b) = object
        [ "id" .= (String $ toPathPiece bid)
        , "currency" .= walletBalanceCurrency b
        , "balance" .= walletBalanceBalance b
        , "bonus_balance" .= walletBalanceBonusBalance b
        ]
