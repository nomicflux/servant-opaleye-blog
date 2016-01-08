{-# LANGUAGE Arrows #-}

module Queries.User where

import Opaleye
import Control.Arrow (returnA)

import App
import Models.User

usersQuery :: Query UserColumn
usersQuery = queryTable userTable

userByEmailQuery :: Email -> Query UserColumn
userByEmailQuery email = proc () -> do
                           user <- usersQuery -< ()
                           restrict -< userEmail user .== pgString email
                           returnA -< user
