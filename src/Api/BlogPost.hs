{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.BlogPost where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)

import App
import Models.BlogPost
import Queries.BlogPost

type BlogPostAPI = Get '[JSON] [BlogPostRead]
              :<|> Capture "id" BlogPostID :> Get '[JSON] (Maybe BlogPostRead)
              :<|> Capture "email" Email :> Get '[JSON] [BlogPostRead]
              :<|> ReqBody '[JSON] BlogPostWrite :> Post '[JSON] (Maybe BlogPostID)

blogPostAPI :: Proxy BlogPostAPI
blogPostAPI = Proxy

blogPostServer :: ServerT BlogPostAPI AppM
blogPostServer = getPosts
            :<|> getPostById
            :<|> getPostsByEmail
            :<|> postPost

getPosts :: AppM [BlogPostRead]
getPosts = do con <- getConn
              addToLogger "I got BlogPosts!"
              liftIO $ O.runQuery con blogPostsQuery

getPostById :: BlogPostID -> AppM (Maybe BlogPostRead)
getPostById postID = do con <- getConn
                        liftIO $ listToMaybe <$> O.runQuery con (blogPostByIdQuery postID)

getPostsByEmail :: Email -> AppM [BlogPostRead]
getPostsByEmail email = do con <- getConn
                           liftIO $ O.runQuery con (blogPostsByEmailQuery email)

postPost :: BlogPostWrite -> AppM (Maybe BlogPostID)
postPost post = do con <- getConn
                   liftIO $ listToMaybe <$>
                     O.runInsertManyReturning con blogPostTable [blogPostToPG post] bpId
