module Handler.Wallets where

import Import
import Data.Time

getWalletsR :: Handler Value
getWalletsR = do
    wallets <- runDB $ selectList [] [] :: Handler [Entity Wallet]
    return $ object ["wallets" .= wallets]

postWalletsR :: Handler ()
postWalletsR = do
    input <- requireJsonBody :: Handler WalletInput
    time <- liftIO $ getZonedTime >>= \time -> return $ zonedTimeToUTC time
    _ <- runDB $ insert $ Wallet time
                                 (walletInputName input)
                                 (walletInputAllowNegativeBalance input)
    sendResponseStatus status201 ("CREATED" :: Text)
