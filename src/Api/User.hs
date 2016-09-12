{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import qualified Database.PostgreSQL.Simple as PGS

import App
import Models.User
import Queries.User

type UserAPI = Get '[JSON] [User]
          :<|> Capture "email" Email :> Get '[JSON] (Maybe User)
          :<|> ReqBody '[JSON] User :> Post '[JSON] (Maybe Email)

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: PGS.Connection -> Server UserAPI
userServer con = getUsers con
            :<|> getUserByEmail con
            :<|> postUser con

getUsers :: PGS.Connection -> AppM [User]
getUsers con = liftIO $ O.runQuery con usersQuery

getUserByEmail :: PGS.Connection -> Email -> AppM (Maybe User)
getUserByEmail con email = liftIO $ listToMaybe <$> O.runQuery con (userByEmailQuery email)

postUser :: PGS.Connection -> User -> AppM (Maybe Email)
postUser con user = listToMaybe <$> liftIO
  (O.runInsertManyReturning con userTable [userToPG user] userEmail)
