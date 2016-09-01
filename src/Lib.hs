{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import System.Log.FastLogger
import Data.Default.Class (def)

import App (Config ( .. ), AppM)
import Api.User
import Api.BlogPost

type API = "users" :> UserAPI
      :<|> "posts" :> BlogPostAPI

openConnection :: IO PGS.Connection
openConnection = PGS.connect PGS.defaultConnectInfo
                 { PGS.connectUser = "blogtutorial"
                 , PGS.connectPassword = "blogtutorial"
                 , PGS.connectDatabase = "blogtutorial"
                 }

startApp :: IO ()
startApp = do
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newStdoutLoggerSet defaultBufSize
  midware <- mkRequestLogger $ def { destination = Logger logger }
  pushLogStrLn logger "Hello World"
  run 8080 $ midware $ app (Config pool logger)

readerTToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerTToExcept pool = Nat (\r -> runReaderT r pool)

app :: Config -> Application
app pool = serve api $ enter (readerTToExcept pool) server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
