module Handler.Transaction where

import Import
import Fields
import Data.Time

getTransactionR :: TransactionId -> Handler Value
getTransactionR tid = do
    trans <- runDB $ get404 tid
    return $ object ["transaction" .= (Entity tid trans)]

putTransactionR :: TransactionId -> Handler ()
putTransactionR tid = do
    input <- requireJsonBody :: Handler TransactionCommand
    trans <- runDB $ get404 tid
    case input of
         CompleteTransaction -> updateTransaction Completed $ Entity tid trans
         CancelTransaction -> updateTransaction Cancelled $ Entity tid trans
         _ -> invalidArgs ["Invalid transaction command"]
    sendResponseStatus status200 ("UPDATED" :: Text)

updateTransaction :: TransactionState -> Entity Transaction -> Handler ()
updateTransaction state (Entity tid trans) = do
    case (transactionState trans) of
         Reserved -> do
             time <- liftIO $ getZonedTime >>= \time -> return $ zonedTimeToUTC time
             runDB $ update tid [TransactionState =. state]
             runDB $ update tid [TransactionStateChanged =. Just time]
         _ -> return ()
