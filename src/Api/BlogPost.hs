{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.BlogPost where

import Servant
import qualified Opaleye as O
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import qualified Database.PostgreSQL.Simple as PGS

import App
import Models.BlogPost
import Queries.BlogPost

type BlogPostAPI = Get '[JSON] [BlogPostRead]
              :<|> Capture "id" BlogPostID :> Get '[JSON] (Maybe BlogPostRead)
              :<|> Capture "email" Email :> Get '[JSON] [BlogPostRead]
              :<|> ReqBody '[JSON] BlogPostWrite :> Post '[JSON] (Maybe BlogPostID)

blogPostAPI :: Proxy BlogPostAPI
blogPostAPI = Proxy

blogPostServer :: PGS.Connection -> Server BlogPostAPI
blogPostServer con = getPosts con
                :<|> getPostById con
                :<|> getPostsByEmail con
                :<|> postPost con

getPosts :: PGS.Connection -> AppM [BlogPostRead]
getPosts con = liftIO $ O.runQuery con blogPostsQuery

getPostById :: PGS.Connection -> BlogPostID -> AppM (Maybe BlogPostRead)
getPostById con postID = liftIO $ listToMaybe <$> O.runQuery con (blogPostByIdQuery postID)

getPostsByEmail :: PGS.Connection -> Email -> AppM [BlogPostRead]
getPostsByEmail con email = liftIO $ O.runQuery con (blogPostsByEmailQuery email)

postPost :: PGS.Connection -> BlogPostWrite -> AppM (Maybe BlogPostID)
postPost con post = liftIO $ listToMaybe <$>
  O.runInsertManyReturning con blogPostTable [blogPostToPG post] bpId
