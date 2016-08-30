
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
startApp = run 8080 app

readerTToExcept :: AppM :~> ExceptT ServantErr IO
readerTToExcept = Nat (\r -> do con <- liftIO $ PGS.connect PGS.defaultConnectInfo
                                                              { PGS.connectUser = "blogtutorial"
                                                              , PGS.connectPassword = "blogtutorial"
                                                              , PGS.connectDatabase = "blogtutorial"
                                                              }
                                runReaderT r con)

app :: Application
app = serve api $ enter readerTToExcept server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
