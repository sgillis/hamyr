module Handler.Balance where

import Import
import Handler.Transactions

getBalanceR :: Text -> Text -> Handler Value
getBalanceR wName cName = do
    b <- runDB $ do
        c <- getBy404 $ UniqueCurrencyName cName
        w <- getBy404 $ UniqueWalletName wName
        getBalance w c
    return $ object ["balance" .= b]
