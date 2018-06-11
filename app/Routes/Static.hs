{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Routes.Static
(
  CrikAPIWithStaticRoute
, crikAPIWithStaticRoute
, staticServer
) where

import Data.Proxy (Proxy(..))
import Prelude hiding (FilePath)
import Servant (Raw, Server, (:<|>)(..), serveDirectoryFileServer)

import Config
import Crik.API
import Crik.Types.File
import Crik.Types.FilePath
import Crik.Types.Library

-- Servant doesn't play nicely with static routes within your own monad so we append it at the end
type CrikAPIWithStaticRoute = CrikAPI :<|> GetRawFile

crikAPIWithStaticRoute :: Proxy CrikAPIWithStaticRoute
crikAPIWithStaticRoute = Proxy

staticServer :: Config -> LibraryId -> Server Raw
staticServer Config{..} LibraryId{..} =
  serveDirectoryFileServer ((unFilePath staticDirectory) ++ "/" ++ (show unLibraryId))
