module App where

import Data.Int (Int64)
import Control.Monad.Trans.Except (ExceptT)
import Servant (Handler)

type BlogPostID = Int64
type Email = String

-- servant-server now defines
-- newtype Handler a = Handler { runHandler' :: ExceptT ServantErr IO a }

type AppM = Handler
