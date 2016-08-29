{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.BlogPost where

import Servant
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.DateTime (DateTime, fromGregorian')

import App

data BlogPost = BlogPost
              { bpId         :: BlogPostID
              , bpTitle      :: String
              , bpBody       :: String
              , bpUsersEmail :: Email
              , bpTimestamp  :: DateTime
              }

instance ToJSON BlogPost where
  toJSON post = object [ "id"        .= bpId post
                       , "title"     .= bpTitle post
                       , "body"      .= bpBody post
                       , "email"     .= bpUsersEmail post
                       , "timestamp" .= bpTimestamp post
                       ]

instance FromJSON BlogPost where
  parseJSON (Object o) = BlogPost <$>
                                  o .: "id" <*>
                                  o .: "title" <*>
                                  o .: "body" <*>
                                  o .: "email" <*>
                                  o .: "timestamp"
  parseJSON _ = mzero

posts :: [BlogPost]
posts = [ BlogPost 1 "First Post" "I don't like apples very much right now." "isaacnewton@gmail.com" (fromGregorian' 1726 4 15)
        , BlogPost 2 "My Blog" "Bored at the patent office, thought I'd start a blog." "alberteinstein@hotmail.com" (fromGregorian' 1903 6 16)]

type BlogPostAPI = Get '[JSON] [BlogPost]
              :<|> Capture "id" BlogPostID :> Get '[JSON] (Maybe BlogPost)
              :<|> Capture "email" Email :> Get '[JSON] [BlogPost]
              :<|> ReqBody '[JSON] BlogPost :> Post '[JSON] [BlogPost]

blogPostAPI :: Proxy BlogPostAPI
blogPostAPI = Proxy

blogPostServer :: Server BlogPostAPI
blogPostServer = getPosts
            :<|> getPostById
            :<|> getPostsByEmail
            :<|> postPost

getPosts :: AppM [BlogPost]
getPosts = return posts

getPostById :: BlogPostID -> AppM (Maybe BlogPost)
getPostById postID = return $ listToMaybe $ filter ((== postID) . bpId) posts

getPostsByEmail :: Email -> AppM [BlogPost]
getPostsByEmail email = return $ filter ((== email) . bpUsersEmail) posts

postPost :: BlogPost -> AppM [BlogPost]
postPost post = return $ posts ++ [post]
