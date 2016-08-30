module App where

import Data.Int (Int64)
import Control.Monad.Trans.Except (ExceptT)
import Servant (ServantErr)

type BlogPostID = Int64
type Email = String

type AppM = ExceptT ServantErr IO
