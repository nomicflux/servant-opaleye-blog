module App where

import Data.Int (Int64)
import Control.Monad.Trans.Either (EitherT)
import Servant (ServantErr)

type BlogPostID = Int64
type Email = String

type AppM = EitherT ServantErr IO
