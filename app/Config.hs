module Config
(
  Config(..)
, ConfigM(..)
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Trans.Except (ExceptT)
import Database.PostgreSQL.Simple (Connection)
import Servant (ServantErr)

data Config = Config { psqlConnection :: Connection }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config (ExceptT ServantErr IO) a }
  deriving (Applicative, Functor, Monad, MonadError ServantErr, MonadIO, MonadReader Config)
