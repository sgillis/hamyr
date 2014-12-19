module Handler.Transactions where

import Import
import Data.Time
import Control.Monad
import Prelude (head)
import Fields
import Database.Persist.Sql
import Get

data ValidatedAmount = ValidatedAmount
    { validatedAmountCurrency :: Entity Currency
    , validatedAmountBalance  :: Maybe (Entity WalletBalance)
    , validatedAmountAmount   :: Double
    }

getTransactionsR :: Handler Value
getTransactionsR = do
    transactions <- runDB $ selectList [] [] :: Handler [Entity Transaction]
    return $ object ["transactions" .= transactions]

postTransactionsR :: Handler ()
postTransactionsR = do
    input <- requireJsonBody :: Handler TransferValue
    success <- runDB $ transferValue input
    if success
       then sendResponseStatus status201 $ object [("message", "Created")]
       else sendResponseStatus status403 $
         object [("message", "Not enough credits")]

transferValue :: YesodPersistBackend site ~ SqlBackend
              => TransferValue -> YesodDB site Bool
transferValue (TransferValue method fw tw as tt cs) = do
    fromWallet <- getBy404Message (String "Did not find from_wallet") $ UniqueWalletName fw
    toWallet <- getBy404 $ UniqueWalletName tw
    transactionType <- getBy404 $ UniqueTransactionTypeName tt
    validatedAmounts <- validateAmount as fromWallet
    let amount = selectAmount fromWallet validatedAmounts cs
    case amount of
         Just a -> do
             time <- lift $
               liftIO $ getZonedTime >>= \time -> return $ zonedTimeToUTC time
             toWalletBalance <- getBalance toWallet (validatedAmountCurrency a)
             createTransaction
                method fromWallet toWallet toWalletBalance
                transactionType a time
             return True
         Nothing -> return False

validateAmount :: YesodPersistBackend site ~ SqlBackend
               => [Amount] -> Entity Wallet -> YesodDB site [ValidatedAmount]
validateAmount as w =
    forM as $ \a -> do
      c <- getBy404 $ UniqueCurrencyName $ currency a
      b <- getBalance w c
      return $ ValidatedAmount c b $ amount a

sufficientBalance :: Entity Wallet -> ValidatedAmount -> Bool
sufficientBalance w a =
    case mb of
         Just (Entity bid b) ->
           if (walletBalanceBalance b) > (validatedAmountAmount a)
              then True
              else False
         Nothing -> True -- Must be a wallet that allows negative balance
    where mb = validatedAmountBalance a

selectAmount :: Entity Wallet -> [ValidatedAmount] -> CurrencySelection
             -> Maybe ValidatedAmount
selectAmount w validatedAmounts cs =
    case cs of
         FirstFull ->
           safeHead $ filter (sufficientBalance w) validatedAmounts

safeHead :: [a] -> Maybe a
safeHead (a:_) = Just a
safeHead [] = Nothing

maybeM :: Monad m => Maybe a -> m () -> (a -> m ()) -> m ()
maybeM value err f = do
    case value of
         Just x  -> f x
         Nothing -> err

createTransaction :: (YesodPersistBackend site ~ SqlBackend)
                  => TransferMethod -> Entity Wallet -> Entity Wallet
                  -> Maybe (Entity WalletBalance) -> Entity TransactionType
                  -> ValidatedAmount -> UTCTime
                  -> YesodDB site ()
createTransaction method fromWallet toWallet toBalance tt amount time = do
    let fromBalance = validatedAmountBalance amount
        trans = Transaction
                    time (Just time) state
                    (entityKey fromWallet) (entityKey toWallet)
                    (validatedAmountAmount amount) 0
                    (entityKey $ validatedAmountCurrency amount) 21
                    (entityKey tt) ("client_name" :: Text)
        state
          | method == Transfer = Completed
          | method == Reserve  = Reserved
    insert trans
    updateWalletBalance fromBalance (-validatedAmountAmount amount)
    updateWalletBalance toBalance (validatedAmountAmount amount)

getBalance :: YesodPersistBackend site ~ SqlBackend
           => Entity Wallet -> Entity Currency
           -> YesodDB site (Maybe (Entity WalletBalance))
getBalance (Entity wid w) (Entity cid c) =
    if (walletAllowNegativeBalance w)
       then return Nothing
       else do
           e <- getBy $ UniqueBalance wid cid
           case e of
                Just b -> return $ Just b
                Nothing -> do
                    let b = WalletBalance wid cid 0 0
                    bid <- insert b
                    return $ Just $ Entity bid b

updateWalletBalance :: YesodPersistBackend site ~ SqlBackend
                    => Maybe (Entity WalletBalance) -> Double
                    -> YesodDB site ()
updateWalletBalance mbal amount =
    maybeM mbal (return ()) $ \bal -> do
        update (entityKey bal) [WalletBalanceBalance +=. amount]
