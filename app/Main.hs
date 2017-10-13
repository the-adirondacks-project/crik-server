module Main where

import Data.Semigroup (Semigroup ((<>)))
import Data.Aeson hiding (json)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple (connectPostgreSQL, query, Connection)
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.FromField (typeOid, FromField(fromField))
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, int4)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (scotty, get, param, status, json)
import Data.Maybe (listToMaybe)
import Network.HTTP.Types.Status (status404)

import Lib

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

main :: IO ()
main = do
  -- Connection info gets passed via environment variables
  psql <- connectPostgreSQL ""
  port <- getPort
  scotty port $ do
    get "/videos/:id" $ do
      id :: Int <- param "id"
      maybeVideo <- liftIO $ getVideo psql (VideoId id)
      case maybeVideo of
        Nothing -> status status404
        Just video -> json video

  print "connected"

newtype VideoId = VideoId { unVideoId :: Int }

instance ToField VideoId where
  toField VideoId{..} = toField unVideoId

instance FromField VideoId where
  fromField field rawData = do
    videoId <- fromField field rawData
    return $ VideoId videoId

instance ToJSON VideoId where
  toJSON VideoId{..} = toJSON unVideoId
  toEncoding VideoId{..} = toEncoding unVideoId

data Video = Video { videoId :: VideoId, videoName :: Text }

instance FromRow Video where
  fromRow = Video <$> field <*> field

instance ToRow Video where
  toRow Video{..} = [toField videoId, toField videoName]

instance ToRow VideoId where
  toRow VideoId{..} = [toField unVideoId]

instance ToJSON Video where
  toJSON Video{..} = object ["id" .= videoId, "name" .= videoName]
  toEncoding Video{..} = pairs ("id" .= videoId <> "name" .= videoName)

getVideo :: Connection -> VideoId -> IO (Maybe Video)
getVideo connection videoId = do
  rows <- query connection "select * from videos where id = ?" (videoId)
  return (listToMaybe rows)
