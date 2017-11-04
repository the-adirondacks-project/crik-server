
import Control.Monad.Reader (runReaderT)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty.Trans (get, json, jsonData, middleware, param, post, put, scottyT, status)

import Config (Config(..), ConfigM(..))
import Routes.Video (setupVideoRoutes)
import Routes.VideoLibrary (setupVideoLibrariesRoutes)

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

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  config <- getConfig
  port <- getPort
  scottyT port (\m -> runReaderT (runConfigM m) config) $ do
    middleware logStdoutDev
    setupVideoRoutes
    setupVideoLibrariesRoutes
