module Cache where

import Prelude
import Data.ByteString
import Data.Text
import Data.Aeson
import Import hiding (get)
import Database.Redis hiding (String)
import qualified Data.ByteString.Lazy as BL

cacheView :: ByteString -> (ByteString -> Maybe a) -> Handler a
          -> (a -> Value) -> Handler Value
cacheView cacheKey parse constructor toValue = do
    cres <- runR $ get cacheKey
    mres <- case cres of
                 Right (Just bs) -> do
                     liftIO $ print bs
                     let parsed = parse bs
                     case parsed of
                          Nothing -> liftIO $ print "Nothing"
                          _       -> liftIO $ print "Success"
                     return parsed
                 _               -> return Nothing
    val <- case mres of
                Just res -> return $ toValue res
                Nothing  -> constructor >>= \res -> return $ toValue res
    _ <- runR $ set cacheKey $ BL.toStrict $ encode val
    return val
