{-# LANGUAGE OverloadedStrings #-}

module Handler.Wallet (getWalletR) where

import Import hiding (get)
import Get
import Data.Aeson
import Cache
import Data.ByteString
import Data.Text.Encoding

getWalletR :: Text -> Handler Value
getWalletR walletname = do
    cacheView cacheKey parse (getWallet walletname) toValue
      where parse = decodeStrict :: ByteString -> Maybe Wallet
            toValue wallet = toJSON wallet
            cacheKey = encodeUtf8 $ "wallet-" <+> walletname

getWallet :: Text -> Handler Wallet
getWallet walletname = do
    Entity _ w <- runDB $ getBy404Message (String errorString) $
      UniqueWalletName walletname
    return w
      where errorString = "Could not find wallet '" <+> walletname <+> "'"
