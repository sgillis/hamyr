module Handler.Wallet where

import Import

getWalletR :: Text -> Handler Value
getWalletR walletname = do
    wallet <- runDB $ getBy404 $ UniqueWalletName walletname
    return $ object ["wallet" .= wallet]
