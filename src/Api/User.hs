{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.User where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.User
import Queries.User

type UserAPI = Get '[JSON] [UserRead]
               :<|> Capture "email" Email :> Get '[JSON] (Maybe UserRead)
               :<|> "verify" :> ReqBody '[JSON] UserWrite :> Post '[JSON] Bool
               :<|> ReqBody '[JSON] UserWrite :> Post '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = getUsers
        :<|> getUserByEmail
        :<|> verifyUser
        :<|> postUser

getUsers :: AppM [UserRead]
getUsers = do con <- getConn
              addToLogger "I got Users!"
              liftIO $ runQuery con usersQuery

getUserByEmail :: Email -> AppM (Maybe UserRead)
getUserByEmail email = do con <- getConn
                          liftIO $ listToMaybe <$> runQuery con (userByEmailQuery email)

verifyUser :: UserWrite -> AppM Bool
verifyUser user = do con <- getConn
                     dbUser <- getUserByEmail (userEmail user)
                     return $ compareUsers dbUser user

postUser :: UserWrite -> AppM Int64
postUser user = do con <- getConn
                   newUser <- liftIO $ userToPG user
                   liftIO $ runInsert con userTable newUser
