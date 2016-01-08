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

type UserAPI = Get '[JSON] [UserRead]
               :<|> Capture "email" Email :> Get '[JSON] (Maybe UserRead)
               :<|> "verify" :> ReqBody '[JSON] UserWrite :> Post '[JSON] Bool
               :<|> ReqBody '[JSON] UserWrite :> Post '[JSON] Int64

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: PGS.Connection -> Server UserAPI
userServer con = getUsers con
            :<|> getUserByEmail con
            :<|> verifyUser con
            :<|> postUser con

getUsers :: PGS.Connection -> AppM [UserRead]
getUsers con = liftIO $ runQuery con usersQuery

getUserByEmail :: PGS.Connection -> Email -> AppM (Maybe UserRead)
getUserByEmail con email = liftIO $ listToMaybe <$> runQuery con (userByEmailQuery email)

verifyUser :: PGS.Connection -> UserWrite -> AppM Bool
verifyUser con user = do dbUser <- getUserByEmail con (userEmail user)
                         return $ compareUsers dbUser user

postUser :: PGS.Connection -> UserWrite -> AppM Int64
postUser con user = do newUser <- liftIO $ userToPG user
                       liftIO $ runInsert con userTable newUser
