module Handler.TransactionTypes where

import Import

getTransactionTypesR :: Handler Value
getTransactionTypesR = do
    tts <- runDB $ selectList [] [] :: Handler [Entity TransactionType]
    return $ object ["transaction_types" .= tts]

postTransactionTypesR :: Handler Html
postTransactionTypesR = do
    tt <- requireJsonBody :: Handler TransactionType
    _  <- runDB $ insert tt
    sendResponseStatus status201 ("CREATED" :: Text)
