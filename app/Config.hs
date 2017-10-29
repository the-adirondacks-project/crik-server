module Config
(
  Config(..)
, ConfigM(..)
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Database.PostgreSQL.Simple (Connection)

data Config = Config { psqlConnection :: Connection }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

