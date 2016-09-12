{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.User where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)

import App
import Models.User
import Queries.User

type UserAPI = Get '[JSON] [UserRead]
               :<|> Capture "email" Email :> Get '[JSON] (Maybe UserRead)
               :<|> "verify" :> ReqBody '[JSON] UserWrite :> Post '[JSON] Bool
               :<|> ReqBody '[JSON] UserWrite :> Post '[JSON] (Maybe Email)

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
              liftIO $ O.runQuery con usersQuery

getUserByEmail :: Email -> AppM (Maybe UserRead)
getUserByEmail email = do con <- getConn
                          liftIO $ listToMaybe <$> O.runQuery con (userByEmailQuery email)

verifyUser :: UserWrite -> AppM Bool
verifyUser user = do dbUser <- getUserByEmail (userEmail user)
                     return $ compareUsers dbUser user

postUser :: UserWrite -> AppM (Maybe Email)
postUser user = do con <- getConn
                   newUser <- liftIO $ userToPG user
                   liftIO $ listToMaybe <$>
                     O.runInsertManyReturning con userTable [newUser] userEmail
