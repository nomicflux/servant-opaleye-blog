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
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool as Pool
import System.Log.FastLogger
import Data.Default.Class (def)
import Data.Maybe (listToMaybe)

import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , Logging ( .. )
           , lookupEnvDefault
           )
import Api.User
import Api.BlogPost

type API = "users" :> UserAPI
      :<|> "posts" :> BlogPostAPI

openConnection :: IO PGS.Connection
openConnection = do
  user <- lookupEnvDefault "SERVANT_PG_USER" "blogtutorial"
  pwd <- lookupEnvDefault "SERVANT_PG_PWD" "blogtutorial"
  db <- lookupEnvDefault "SERVANT_PG_DB" "blogtutorial"
  PGS.connect PGS.defaultConnectInfo
                 { PGS.connectUser = user
                 , PGS.connectPassword = pwd
                 , PGS.connectDatabase = db
                 }

makeLogger :: Environment -> Logging -> IO (LoggerSet, Middleware)
makeLogger env logPlace =
  let loggerM = case logPlace of
        STDOut -> newStdoutLoggerSet defaultBufSize
        STDErr -> newStderrLoggerSet defaultBufSize
        File filename -> newFileLoggerSet defaultBufSize filename
  in do
    logger <- loggerM
    midware <- case env of
      Test -> return id
      Production -> mkRequestLogger $ def { destination = Logger logger
                                          , outputFormat = Apache FromSocket
                                          }
      Development -> mkRequestLogger $ def { destination = Logger logger }
    return (logger, midware)

startApp :: [String] -> IO ()
startApp args = do
  let mfilename = listToMaybe args
      logDefault = case mfilename of
        Nothing -> STDOut
        Just filename -> File filename
  port <- lookupEnvDefault "SERVANT_PORT" 8080
  env <- lookupEnvDefault "SERVANT_ENV" Production
  logPlace <- lookupEnvDefault "SERVANT_LOG" logDefault
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  (logger, midware)  <- makeLogger env logPlace
  pushLogStrLn logger $ toLogStr $
    "Listening on port " ++ show port ++ " at level " ++ show env ++ " and logging to "  ++ show logPlace ++ " with args " ++ unwords args
  run port $ midware $ app (Config pool logger)

readerTToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerTToExcept pool = Nat (\r -> runReaderT r pool)

app :: Config -> Application
app pool = serve api $ enter (readerTToExcept pool) server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = userServer
    :<|> blogPostServer
