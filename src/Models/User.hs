{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.User where

import qualified Opaleye as O
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
type UserColumn = User' (O.Column O.PGText) (O.Column O.PGBytea)

$(makeAdaptorAndInstance "pUser" ''User')

instance ToJSON UserRead where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON UserWrite where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              o .: "password"
  parseJSON _ = mzero

userTable :: O.Table UserColumn UserColumn
userTable = O.Table "users" (pUser User { userEmail = O.required "email"
                                        , userPassword = O.required "password"
                                        })

userToPG :: UserWrite -> IO UserColumn
userToPG user = do hashedPwd <- flip makePassword 12 . BS.pack . userPassword $ user
                   return
                     User { userEmail = O.pgString . userEmail $ user
                          , userPassword = O.pgStrictByteString hashedPwd
                          }

compareUsers :: Maybe UserRead -> UserWrite -> Bool
compareUsers Nothing _ = False
compareUsers (Just dbUser) userAttempt = verifyPassword (BS.pack . userPassword $ userAttempt)
                                                        (userPassword dbUser)
