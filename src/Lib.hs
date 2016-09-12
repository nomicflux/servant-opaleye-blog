{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:<|>)( .. ), (:>))
import qualified Servant as S

import Api.User
import Api.BlogPost

type API = "users" :> UserAPI
           :<|> "posts" :> BlogPostAPI

startApp :: IO ()
startApp = Warp.run 8080 app

app :: Wai.Application
app = S.serve api server

api :: S.Proxy API
api = S.Proxy

server :: S.Server API
server = userServer
  :<|> blogPostServer
