
import Control.Monad.Reader (runReaderT)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty.Trans (get, json, jsonData, middleware, param, post, put, scottyT, status)

import Data.Proxy (Proxy(..))
import Servant (Handler, Server, serve)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Config (Config(..), ConfigM(..))
import Routes (API)
import Routes.Video (VideoAPI, setupVideoRoutes)
import Routes.VideoLibrary (setupVideoLibrariesRoutes)

import Types.Video (Video(Video), VideoId(VideoId))

maybeGetPort :: IO (Maybe Int)
maybeGetPort = do
  maybeRawPort <- lookupEnv "PORT"
  case maybeRawPort of
    Nothing -> return Nothing
    Just rawPort -> return (readMaybe rawPort :: Maybe Int)

getPort :: IO (Int)
getPort = do
  maybePort <- maybeGetPort
  case maybePort of
    Nothing -> return 8015
    Just x -> return x

getConfig :: IO (Config)
getConfig = do
  psqlConnection <- connectPostgreSQL ""
  return $ Config psqlConnection

handleVideo :: Int -> Handler (Video VideoId)
handleVideo videoId = return (Video (VideoId videoId) "foo")

api :: Proxy API
api = Proxy

server :: Server VideoAPI
server = handleVideo

application :: Application
application = serve api server

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  config <- getConfig
  port <- getPort
  run port (logStdoutDev application)
  {--
  scottyT port (\m -> runReaderT (runConfigM m) config) $ do
    middleware logStdoutDev
    setupVideoRoutes
    setupVideoLibrariesRoutes
  --}
  return ()
