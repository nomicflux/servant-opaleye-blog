{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.User where

import Opaleye
import Control.Monad (mzero)
import Crypto.PasswordStore
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import App

data User' email pwd = User
                         { userEmail    :: email
                         , userPassword :: pwd
                         }
type UserRead = User' Email ByteString
type UserWrite = User' Email String
type UserColumn = User' (Column PGText) (Column PGBytea)

$(makeAdaptorAndInstance "pUser" ''User')

instance ToJSON UserRead where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON UserWrite where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              o .: "password"
  parseJSON _ = mzero

userTable :: Table UserColumn UserColumn
userTable = Table "users" (pUser User { userEmail = required "email"
                                      , userPassword = required "password"
                                      })

userToPG :: UserWrite -> IO UserColumn
userToPG user = do hashedPwd <- flip makePassword 12 . BS.pack . userPassword $ user
                   return
                     User { userEmail = pgString . userEmail $ user
                          , userPassword = pgStrictByteString hashedPwd
                          }

compareUsers :: Maybe UserRead -> UserWrite -> Bool
compareUsers Nothing _ = False
compareUsers (Just dbUser) userAttempt = verifyPassword (BS.pack . userPassword $ userAttempt)
                                                        (userPassword dbUser)
