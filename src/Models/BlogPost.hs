{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.BlogPost where

import Opaleye
import Control.Monad (mzero)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import App

data BlogPost' id title body email time = BlogPost
                                            { bpId         :: id
                                            , bpTitle      :: title
                                            , bpBody       :: body
                                            , bpUsersEmail :: email
                                            , bpTimestamp  :: time
                                            }

type BlogPostRead = BlogPost' BlogPostID String String Email DateTime
type BlogPostWrite = BlogPost' (Maybe BlogPostID) String String Email (Maybe DateTime)
type BPColumnRead = BlogPost' (Column PGInt8)
                              (Column PGText)
                              (Column PGText)
                              (Column PGText)
                              (Column PGTimestamptz)
type BPColumnWrite = BlogPost' (Maybe (Column PGInt8))
                               (Column PGText)
                               (Column PGText)
                               (Column PGText)
                               (Maybe (Column PGTimestamptz))

instance ToJSON BlogPostRead where
  toJSON post = object [ "id"        .= bpId post
                       , "title"     .= bpTitle post
                       , "body"      .= bpBody post
                       , "email"     .= bpUsersEmail post
                       , "timestamp" .= bpTimestamp post
                       ]

instance FromJSON BlogPostWrite where
  parseJSON (Object o) = BlogPost <$>
                                  o .:? "id" <*>
                                  o .: "title" <*>
                                  o .: "body" <*>
                                  o .: "email" <*>
                                  o .:? "timestamp"
  parseJSON _ = mzero

$(makeAdaptorAndInstance "pBlogPost" ''BlogPost')

blogPostTable :: Table BPColumnWrite BPColumnRead
blogPostTable = Table "posts" (pBlogPost BlogPost { bpId = optional "id"
                                                  , bpTitle = required "title"
                                                  , bpBody = required "body"
                                                  , bpUsersEmail = required "users_email"
                                                  , bpTimestamp = optional "timestamp"
                                                  })

blogPostToPG :: BlogPostWrite -> BPColumnWrite
blogPostToPG = pBlogPost BlogPost { bpId = const Nothing
                                  , bpTitle = pgString
                                  , bpBody = pgString
                                  , bpUsersEmail = pgString
                                  , bpTimestamp = const Nothing
                                  }
