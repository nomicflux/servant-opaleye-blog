
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Database.PostgreSQL.Simple as PGS

import Api.User
import Api.BlogPost

type API = "users" :> UserAPI
      :<|> "posts" :> BlogPostAPI

startApp :: IO ()
startApp = do
             con <- PGS.connect PGS.defaultConnectInfo
                                { PGS.connectUser = "blogtutorial"
                                , PGS.connectPassword = "blogtutorial"
                                , PGS.connectDatabase = "blogtutorial"
                                }
             run 8080 $ app con

app :: PGS.Connection -> Application
app con = serve api $ server con

api :: Proxy API
api = Proxy

server :: PGS.Connection -> Server API
server con = userServer con
        :<|> blogPostServer con
