{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.BlogPost where

import qualified Opaleye as O
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
type BPColumnRead = BlogPost' (O.Column O.PGInt8)
                              (O.Column O.PGText)
                              (O.Column O.PGText)
                              (O.Column O.PGText)
                              (O.Column O.PGTimestamptz)
type BPColumnWrite = BlogPost' (Maybe (O.Column O.PGInt8))
                               (O.Column O.PGText)
                               (O.Column O.PGText)
                               (O.Column O.PGText)
                               (Maybe (O.Column O.PGTimestamptz))

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

blogPostTable :: O.Table BPColumnWrite BPColumnRead
blogPostTable = O.Table "posts" (pBlogPost BlogPost { bpId = O.optional "id"
                                                    , bpTitle = O.required "title"
                                                    , bpBody = O.required "body"
                                                    , bpUsersEmail = O.required "users_email"
                                                    , bpTimestamp = O.optional "timestamp"
                                                    })

blogPostToPG :: BlogPostWrite -> BPColumnWrite
blogPostToPG = pBlogPost BlogPost { bpId = const Nothing
                                  , bpTitle = O.pgString
                                  , bpBody = O.pgString
                                  , bpUsersEmail = O.pgString
                                  , bpTimestamp = const Nothing
                                  }
