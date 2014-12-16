module Handler.Balance where

import Import
import Handler.Transactions

getBalanceR :: Text -> Text -> Handler Value
getBalanceR wName cName = do
    c <- runDB $ getBy404 $ UniqueCurrencyName cName
    w <- runDB $ getBy404 $ UniqueWalletName wName
    b <- getBalance w c
    return $ object ["balance" .= b]
