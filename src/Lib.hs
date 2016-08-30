
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PGS

import App
import Api.User
import Api.BlogPost

type API = "users" :> UserAPI
      :<|> "posts" :> BlogPostAPI

startApp :: IO ()
startApp = do
  con <- liftIO $ PGS.connect PGS.defaultConnectInfo
         { PGS.connectUser = "blogtutorial"
         , PGS.connectPassword = "blogtutorial"
         , PGS.connectDatabase = "blogtutorial"
         }
  run 8080 (app con)

readerTToExcept :: PGS.Connection -> AppM :~> ExceptT ServantErr IO
readerTToExcept con = Nat (\r -> runReaderT r con)

app :: PGS.Connection -> Application
app con = serve api $ enter (readerTToExcept con) server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
