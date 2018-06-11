{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config
(
  Config(..)
, ConfigM(..)
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Database.PostgreSQL.Simple (Connection)
import Prelude hiding (FilePath)
import Servant (Handler, ServantErr)

import Crik.Types.FilePath

data Config = Config {
  psqlConnection :: Connection
, staticDirectory :: FilePath
}

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config Handler a }
  deriving (Applicative, Functor, Monad, MonadError ServantErr, MonadIO, MonadReader Config)
