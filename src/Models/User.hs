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

import App

data User' email pwd = User
                         { userEmail    :: email
                         , userPassword :: pwd
                         }
type User = User' Email ByteString
type UserColumn = User' (Column PGText) (Column PGBytea)

$(makeAdaptorAndInstance "pUser" ''User')

instance ToJSON User where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON User where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              (BS.pack <$> o .: "password")
  parseJSON _ = mzero

userTable :: Table UserColumn UserColumn
userTable = Table "users" (pUser User { userEmail = required "email"
                                      , userPassword = required "password"
                                      })

userToPG :: User -> UserColumn
userToPG = pUser User { userEmail = pgString
                      , userPassword = pgStrictByteString
                      }
