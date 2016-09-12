
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:<|>)( .. ), (:>), (:~>))
import qualified Servant as S
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
  Warp.run 8080 (app con)

readerTToExcept :: PGS.Connection -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept con = S.Nat (\r -> runReaderT r con)

app :: PGS.Connection -> Wai.Application
app con = S.serve api $ S.enter (readerTToExcept con) server

api :: S.Proxy API
api = S.Proxy

server :: S.ServerT API AppM
server = userServer
    :<|> blogPostServer
