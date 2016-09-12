{-# LANGUAGE Arrows #-}

module Queries.User where

import qualified Opaleye as O
import Opaleye ((.==))
import Control.Arrow (returnA)

import App
import Models.User

usersQuery :: O.Query UserColumn
usersQuery = O.queryTable userTable

userByEmailQuery :: Email -> O.Query UserColumn
userByEmailQuery email = proc () -> do
  user <- usersQuery -< ()
  O.restrict -< userEmail user .== O.pgString email
  returnA -< user
