{-# LANGUAGE OverloadedStrings #-}

module Redis where

import Prelude
import Database.Redis
import Yesod
import Data.Text

mkRedisConn :: IO Connection
mkRedisConn = connect $ defaultConnectInfo { connectPort = PortNumber 6380 }

defaultRunR :: (site -> Connection) -> Redis a -> HandlerT site IO a
defaultRunR getConn f = do
    master <- getYesod
    let conn = getConn master
    liftIO $ runRedis conn f
