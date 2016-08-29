{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as PGS

import App
import Models.User
import Queries.User

type UserAPI = Get '[JSON] [User]
          :<|> Capture "email" Email :> Get '[JSON] (Maybe User)
          :<|> ReqBody '[JSON] User :> Post '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: PGS.Connection -> Server UserAPI
userServer con = getUsers con
            :<|> getUserByEmail con
            :<|> postUser con

getUsers :: PGS.Connection -> AppM [User]
getUsers con = liftIO $ runQuery con usersQuery

getUserByEmail :: PGS.Connection -> Email -> AppM (Maybe User)
getUserByEmail con email = liftIO $ listToMaybe <$> runQuery con (userByEmailQuery email)

postUser :: PGS.Connection -> User -> AppM Int64
postUser con user = liftIO $ runInsert con userTable $ userToPG user
