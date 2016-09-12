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
import Data.Default.Class (def, Default)
import Data.Maybe (listToMaybe)

import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , LogTo ( .. )
           , lookupEnvDefault
           )
import Api.User
import Api.BlogPost

type API = "users" :> UserAPI
      :<|> "posts" :> BlogPostAPI

data ConnectionInfo = ConnectionInfo
                      { connUser :: String
                      , connPassword :: String
                      , connDatabase :: String
                      }

instance Default ConnectionInfo where
  def = ConnectionInfo
        { connUser = "blogtutorial"
        , connPassword = "blogtutorial"
        , connDatabase = "blogtutorial"
        }

getConnInfo :: IO ConnectionInfo
getConnInfo =
  ConnectionInfo <$>
    lookupEnvDefault "SERVANT_PG_USER" (connUser def) <*>
    lookupEnvDefault "SERVANT_PG_PWD" (connPassword def) <*>
    lookupEnvDefault "SERVANT_PG_DB" (connDatabase def)

connInfoToPG :: ConnectionInfo -> PGS.ConnectInfo
connInfoToPG connInfo = PGS.defaultConnectInfo
                        { PGS.connectUser = connUser connInfo
                        , PGS.connectPassword = connPassword connInfo
                        , PGS.connectDatabase = connDatabase connInfo
                        }

openConnection :: IO PGS.Connection
openConnection = do
  connInfo <- getConnInfo
  PGS.connect (connInfoToPG connInfo)

makeLogger :: LogTo -> IO FL.LoggerSet
makeLogger logTo = case logTo of
        STDOut -> FL.newStdoutLoggerSet FL.defaultBufSize
        STDErr -> FL.newStderrLoggerSet FL.defaultBufSize
        File filename -> FL.newFileLoggerSet FL.defaultBufSize filename

makeMiddleware :: FL.LoggerSet -> Environment -> IO Wai.Middleware
makeMiddleware logger env = case env of
      Test -> return id
      Production -> MidRL.mkRequestLogger $ def { MidRL.destination = MidRL.Logger logger
                                                , MidRL.outputFormat = MidRL.Apache MidRL.FromSocket
                                                }
      Development -> MidRL.mkRequestLogger $ def { MidRL.destination = MidRL.Logger logger }

startApp :: [String] -> IO ()
startApp args = do
  port <- lookupEnvDefault "SERVANT_PORT" 8080
  env <- lookupEnvDefault "SERVANT_ENV" Production
  logTo <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> lookupEnvDefault "SERVANT_LOG" STDOut
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- makeLogger logTo
  midware <- makeMiddleware logger env
  FL.pushLogStrLn logger $ FL.toLogStr $
    "Listening on port " ++ show port ++ " at level " ++ show env ++ " and logging to "  ++ show logTo ++ " with args " ++ unwords args
  Warp.run port $ midware $ app (Config pool logger)

readerTToExcept :: Config -> AppM :~> ExceptT S.ServantErr IO
readerTToExcept pool = S.Nat (\r -> runReaderT r pool)

app :: Config -> Wai.Application
app pool = S.serve api $ S.enter (readerTToExcept pool) server

api :: S.Proxy API
api = S.Proxy

server :: S.ServerT API AppM
server = userServer
    :<|> blogPostServer
