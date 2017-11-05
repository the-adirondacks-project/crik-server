module Routes
(
  API
) where

import Servant.API ((:>))

import Routes.Video (VideoAPI)

type API = "api" :> VideoAPI
