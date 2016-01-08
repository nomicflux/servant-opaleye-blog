{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.BlogPost where

import Servant
import Opaleye
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import qualified Database.PostgreSQL.Simple as PGS

import App
import Models.BlogPost
import Queries.BlogPost

type BlogPostAPI = Get '[JSON] [BlogPostRead]
              :<|> Capture "id" BlogPostID :> Get '[JSON] (Maybe BlogPostRead)
              :<|> Capture "email" Email :> Get '[JSON] [BlogPostRead]
              :<|> ReqBody '[JSON] BlogPostWrite :> Post '[JSON] Int64

blogPostAPI :: Proxy BlogPostAPI
blogPostAPI = Proxy

blogPostServer :: PGS.Connection -> Server BlogPostAPI
blogPostServer con = getPosts con
                :<|> getPostById con
                :<|> getPostsByEmail con
                :<|> postPost con

getPosts :: PGS.Connection -> AppM [BlogPostRead]
getPosts con = liftIO $ runQuery con blogPostsQuery

getPostById :: PGS.Connection -> BlogPostID -> AppM (Maybe BlogPostRead)
getPostById con id = liftIO $ listToMaybe <$> runQuery con (blogPostByIdQuery id)

getPostsByEmail :: PGS.Connection -> Email -> AppM [BlogPostRead]
getPostsByEmail con email = liftIO $ runQuery con (blogPostsByEmailQuery email)

postPost :: PGS.Connection -> BlogPostWrite -> AppM Int64
postPost con post = liftIO $ runInsert con blogPostTable $ blogPostToPG post
