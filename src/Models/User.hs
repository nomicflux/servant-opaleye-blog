{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.User where

import qualified Opaleye as O
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
type UserColumn = User' (O.Column O.PGText) (O.Column O.PGBytea)

$(makeAdaptorAndInstance "pUser" ''User')

instance ToJSON User where
  toJSON user = object [ "email" .= userEmail user ]

instance FromJSON User where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              (BS.pack <$> o .: "password")
  parseJSON _ = mzero

userTable :: O.Table UserColumn UserColumn
userTable = O.Table "users" (pUser User { userEmail = O.required "email"
                                        , userPassword = O.required "password"
                                        })

userToPG :: User -> UserColumn
userToPG = pUser User { userEmail = O.pgString
                      , userPassword = O.pgStrictByteString
                      }
