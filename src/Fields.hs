module Fields where

import Prelude
import Database.Persist.TH
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Text

data TransactionState = Completed | Reserved | Cancelled
    deriving (Show, Eq, Read)
derivePersistField "TransactionState"

instance ToJSON TransactionState where
    toJSON Completed = AT.String "completed"
    toJSON Reserved = AT.String "reserved"
    toJSON Cancelled = AT.String "cancelled"
