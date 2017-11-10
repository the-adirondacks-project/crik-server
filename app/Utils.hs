module Utils
(
  makeHandler
) where

import Control.Monad.Reader (runReaderT)
import Servant (Handler, (:~>)(NT))

import Config(Config, ConfigM(..))

makeHandler :: Config -> ConfigM :~> Handler
makeHandler config = NT (\m -> runReaderT (runConfigM m) config)
