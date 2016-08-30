module App where

import Data.Int (Int64)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Servant (ServantErr)
import Database.PostgreSQL.Simple (Connection)

type BlogPostID = Int64
type Email = String

type AppM = ReaderT Connection (ExceptT ServantErr IO)
