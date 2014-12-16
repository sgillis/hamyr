module Handler.Currency where

import Import

getCurrencyR :: CurrencyId -> Handler Value
getCurrencyR cid = do
    cur <- runDB $ get404 cid
    return $ object ["currency" .= (Entity cid cur)]
