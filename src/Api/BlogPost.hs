{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.BlogPost where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)

import App
import Models.BlogPost
import Queries.BlogPost

type BlogPostAPI = Get '[JSON] [BlogPostRead]
              :<|> Capture "id" BlogPostID :> Get '[JSON] (Maybe BlogPostRead)
              :<|> Capture "email" Email :> Get '[JSON] [BlogPostRead]
              :<|> ReqBody '[JSON] BlogPostWrite :> Post '[JSON] Int64

blogPostAPI :: Proxy BlogPostAPI
blogPostAPI = Proxy

blogPostServer :: ServerT BlogPostAPI AppM
blogPostServer = getPosts
            :<|> getPostById
            :<|> getPostsByEmail
            :<|> postPost

getPosts :: AppM [BlogPostRead]
getPosts = do con <- ask >>= getConn
              liftIO $ runQuery con blogPostsQuery

getPostById :: BlogPostID -> AppM (Maybe BlogPostRead)
getPostById postID = do con <- ask >>= getConn
                        liftIO $ listToMaybe <$> runQuery con (blogPostByIdQuery postID)

getPostsByEmail :: Email -> AppM [BlogPostRead]
getPostsByEmail email = do con <- ask >>= getConn
                           liftIO $ runQuery con (blogPostsByEmailQuery email)

postPost :: BlogPostWrite -> AppM Int64
postPost post = do con <- ask >>= getConn
                   liftIO $ runInsert con blogPostTable $ blogPostToPG post
