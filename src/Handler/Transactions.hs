{-# LANGUAGE RankNTypes #-}

module Handler.Transactions where

import Import
import Data.Time
import Control.Monad
import Prelude (head)
import Fields

data ValidatedAmount = ValidatedAmount
    { getCurrency :: Entity Currency
    , getAmount   :: Double
    }
    deriving Show

getTransactionsR :: Handler Value
getTransactionsR = do
    transactions <- runDB $ selectList [] [] :: Handler [Entity Transaction]
    return $ object ["transactions" .= transactions]

postTransactionsR :: Handler ()
postTransactionsR = do
    input <- requireJsonBody :: Handler TransactionCommand
    success <- transferValue input
    if success
       then sendResponseStatus status201 ("CREATED" :: Text)
       else sendResponseStatus status403 ("Unable to execute transaction" :: Text)

transferValue :: TransactionCommand -> Handler Bool
transferValue (TransferValue fw tw as tt cs) = do
    fromWallet <- runDB $ getBy404 $ UniqueWalletName fw
    toWallet <- runDB $ getBy404 $ UniqueWalletName tw
    transactionType <- runDB $ getBy404 $ UniqueTransactionTypeName tt
    validatedAmounts <- validateAmount as
    amount <- selectAmount fromWallet validatedAmounts cs
    case amount of
         Just a -> do
             createTransaction Transfer fromWallet toWallet transactionType a
             return True
         Nothing -> return False
transferValue (ReserveValue fw tw as tt cs) = do
    fromWallet <- runDB $ getBy404 $ UniqueWalletName fw
    toWallet <- runDB $ getBy404 $ UniqueWalletName tw
    transactionType <- runDB $ getBy404 $ UniqueTransactionTypeName tt
    validatedAmounts <- validateAmount as
    amount <- selectAmount fromWallet validatedAmounts cs
    case amount of
         Just a -> do
             createTransaction Reserve fromWallet toWallet transactionType a
             return True
         Nothing -> return False
transferValue _ = invalidArgs ["Invalid transaction command"]

validateAmount :: [Amount] -> Handler [ValidatedAmount]
validateAmount as = do
    if (length as == 0)
       then invalidArgs ["Must provide amounts"]
       else return ()
    forM as $ \a -> do
      c <- runDB $ getBy404 $ UniqueCurrencyName $ currency a
      return $ ValidatedAmount c $ amount a

getBalance :: Entity Wallet -> Entity Currency
           -> Handler (Maybe (Entity WalletBalance))
getBalance (Entity wid w) (Entity cid c) =
    if (walletAllowNegativeBalance w)
       then return Nothing
       else do
           e <- runDB $ getBy $ UniqueBalance wid cid
           case e of
                Just b -> return $ Just b
                Nothing -> do
                    let b = WalletBalance wid cid 0 0
                    bid <- runDB $ insert b
                    return $ Just $ Entity bid b

sufficientBalance :: Entity Wallet -> ValidatedAmount -> Handler Bool
sufficientBalance w a = do
    mb <- getBalance w $ getCurrency a
    case mb of
         Just (Entity bid b) -> if (walletBalanceBalance b) > (getAmount a)
                                   then return True
                                   else return False
         Nothing -> return True

selectAmount :: Entity Wallet -> [ValidatedAmount] -> CurrencySelection
             -> Handler (Maybe ValidatedAmount)
selectAmount w validatedAmounts cs =
    case cs of
         FirstFull -> do
             as <- filterM (sufficientBalance w) validatedAmounts
             return $ safeHead as

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead [] = Nothing

maybeM :: Monad m => Maybe a -> m () -> (a -> m ()) -> m ()
maybeM value err f = do
    case value of
         Just x  -> f x
         Nothing -> err

createTransaction :: TransferMethod -> Entity Wallet -> Entity Wallet
                  -> Entity TransactionType -> ValidatedAmount -> Handler ()
createTransaction method fromWallet toWallet tt amount = do
    time <- liftIO $ getZonedTime >>= \time -> return $ zonedTimeToUTC time
    fromBalance <- getBalance fromWallet (getCurrency amount)
    toBalance   <- getBalance toWallet (getCurrency amount)
    let trans = Transaction
                    time (Just time) state
                    (entityKey fromWallet) (entityKey toWallet)
                    (getAmount amount) 0
                    (entityKey $ getCurrency amount) 21
                    (entityKey tt) ("client_name" :: Text)
    _ <- runDB $ insert trans
    updateWalletBalance fromBalance (-getAmount amount)
    updateWalletBalance toBalance (getAmount amount)
    return ()
      where
          state
            | method == Transfer = Completed
            | method == Reserve  = Reserved

updateWalletBalance :: Maybe (Entity WalletBalance) -> Double
                    -> Handler ()
updateWalletBalance mbal amount =
    maybeM mbal (return ()) $ \bal -> do
        runDB $ update (entityKey bal) [WalletBalanceBalance +=. amount]
        return ()
