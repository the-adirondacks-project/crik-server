
import Control.Monad.Reader (runReaderT)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty.Trans (get, json, jsonData, middleware, param, post, put, scottyT, status)

import Data.Proxy (Proxy(..))
import Servant (Handler, Server, (:~>)(..), (:<|>)(..), enter, serve, throwError, err404)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Control.Monad.Reader (asks, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Video (getAllVideos, getVideoById, insertVideo, updateVideo)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader)

import Config (Config(..), ConfigM(..))
import Routes (API)
import Routes.Video
  (
    VideoAPI
  , getVideo
  , getVideoFileHandler
  , getVideoFilesForVideoHandler
  , getVideoFilesHandler
  , getVideos
  , newVideoHandler
  , setupVideoRoutes
  )
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

api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = enter nt $
  getVideos :<|>
  newVideoHandler :<|>
  getVideo :<|>
  getVideoFilesForVideoHandler :<|>
  getVideoFilesHandler :<|>
  getVideoFileHandler
  where nt = NT $ (\m -> runReaderT (runConfigM m) config)

app :: Config -> Application
app config = serve api (server config)

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  config <- getConfig
  port <- getPort
  run port (logStdoutDev (app config))
  {--
  scottyT port (\m -> runReaderT (runConfigM m) config) $ do
    middleware logStdoutDev
    setupVideoRoutes
    setupVideoLibrariesRoutes
  --}
  return ()
