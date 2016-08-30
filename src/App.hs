module App where

import Data.Int (Int64)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Servant (ServantErr)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool, withResource)

type BlogPostID = Int64
type Email = String

type DBPool = Pool Connection

type AppM = ReaderT DBPool (ExceptT ServantErr IO)

getConnFromPool :: DBPool -> AppM Connection
getConnFromPool pool = withResource pool return

getConn :: AppM Connection
getConn = ask >>= getConnFromPool
