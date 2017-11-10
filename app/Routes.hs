module Routes
(
  API
) where

import Servant.API ((:>), (:<|>))

import Routes.Video (VideoAPI)
import Routes.VideoLibrary (VideoLibraryAPI)

type API = "api" :> (VideoAPI :<|> VideoLibraryAPI)
