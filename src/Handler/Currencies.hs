module Handler.Currencies where

import Import
import Data.Time

getCurrenciesR :: Handler Value
getCurrenciesR = do
    currencies <- runDB $ selectList [] [] :: Handler [Entity Currency]
    return $ object ["currencies" .= currencies]

postCurrenciesR :: Handler ()
postCurrenciesR = do
    input <- requireJsonBody :: Handler CurrencyInput
    time <- liftIO $ getZonedTime >>= \time -> return $ zonedTimeToUTC time
    _ <- runDB $ insert $ Currency time
                                   (currencyInputName input)
                                   (currencyInputUnit input)
    sendResponseStatus status201 ("CREATED" :: Text)
