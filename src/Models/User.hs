{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.User where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Profunctor as P

import App

data User' email pwd = User
                         { userEmail    :: email
                         , userPassword :: pwd
                         }
type User = User' Email ByteString
type UserColumn = User' (Column PGText) (Column PGBytea)

class UserType u where
  getEmail :: u a b -> a
  getPassword :: u a b -> b

instance UserType User' where
  getEmail (User e _) = e
  getPassword (User _ p) = p

data UserWrite a b = UserWrite a

instance UserType UserWrite where
  getEmail (UserWrite e) = e
  getPassword _ = undefined

$(makeAdaptorAndInstance "pUser" ''User')

instance ToJSON User where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON User where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              (BS.pack <$> o .: "password")
  parseJSON _ = mzero

userTable' :: Table UserColumn UserColumn
userTable' = Table "users" (pUser User { userEmail = required "email"
                                      , userPassword = required "password"
                                      })

userTable :: Table UserColumn UserColumn
userTable = Table "users" (User <$> P.lmap getEmail (required "email") <*> P.lmap getPassword (required "password"))

userToPG' :: User -> UserColumn
userToPG' = pUser User { userEmail = pgString
                      , userPassword = pgStrictByteString
                      }

userToPG :: UserWrite Email b -> UserColumn
userToPG (UserWrite e)= User { userEmail = pgString e
                             , userPassword = pgStrictByteString (BS.pack "")
                             }
