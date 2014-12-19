module Get where

import Prelude
import Control.Monad.Reader
import Database.Persist.Class
import Data.Aeson
import Network.HTTP.Types (status404)
import Yesod.Core
import Database.Persist.Types
import Data.Text

getBy404Message :: ( PersistUnique (PersistEntityBackend val)
                   , PersistEntity val, MonadHandler m)
                => Value -> Unique val
                -> ReaderT (PersistEntityBackend val) m (Entity val)
getBy404Message message key = do
    mres <- getBy key
    case mres of
         Nothing -> sendResponseStatus status404 $ object
                        [ ("error_code", Number 404)
                        , ("message", message)
                        ]
         Just res -> return res

(<+>) :: Text -> Text -> Text
(<+>) = append
