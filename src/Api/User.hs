{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.User where

import Servant
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (listToMaybe)

import App

data User = User
  { userEmail    :: Email
  , userPassword :: String
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON User where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              o .: "password"
  parseJSON _ = mzero

users :: [User]
users = [ User "isaacnewton@gmail.com" "betterthanleibniz"
        , User "alberteinstein@hotmail.com" "crazytrain"
        ]

type UserAPI = Get '[JSON] [User]
               :<|> Capture "email" Email :> Get '[JSON] (Maybe User)
               :<|> ReqBody '[JSON] User :> Post '[JSON] [User]

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: Server UserAPI
userServer = getUsers
        :<|> getUserByEmail
        :<|> postUser

getUsers :: AppM [User]
getUsers = return users

getUserByEmail :: Email -> AppM (Maybe User)
getUserByEmail email = return $ listToMaybe $ filter ((== email) . userEmail) users

postUser :: User -> AppM [User]
postUser user = return $ users ++ [user]
