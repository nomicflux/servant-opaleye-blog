{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import Servant ((:<|>)( .. ), (:>), (:~>))
import qualified Servant as S
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import qualified System.Log.FastLogger as FL
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
  logger <- FL.newStdoutLoggerSet FL.defaultBufSize
  midware <- MidRL.mkRequestLogger $ def { MidRL.destination = MidRL.Logger logger }
  FL.pushLogStrLn logger "Hello World"
  Warp.run 8080 $ midware $ app (Config pool logger)

readerTToExcept :: Config -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept pool = S.Nat (\r -> runReaderT r pool)

app :: Config -> Wai.Application
app pool = S.serve api $ S.enter (readerTToExcept pool) server

api :: S.Proxy API
api = S.Proxy

server :: S.ServerT API AppM
server = userServer
    :<|> blogPostServer
