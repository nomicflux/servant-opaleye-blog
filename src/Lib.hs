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
import Data.Default.Class (def, Default)
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
getConnInfo = do
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
  port <- lookupEnvDefault "SERVANT_PORT" 8080
  env <- lookupEnvDefault "SERVANT_ENV" Production
  logPlace <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> lookupEnvDefault "SERVANT_LOG" STDOut
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
