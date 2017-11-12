{-# LANGUAGE OverloadedLists #-}
import Control.Monad.Reader (runReaderT)
import Data.Swagger
import Data.Proxy (Proxy(..))
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Handler, Server, (:<|>)(..), (:~>)(NT), enter, serve)
import Servant.Swagger
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Config (Config(..), ConfigM(..))
import Routes (API)
import Routes.Video (videoServer)
import Routes.VideoLibrary (videoLibraryServer)

import Types.Video
import Types.VideoLibrary
import Types.VideoFile
import Data.Aeson.Encode
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Lens
import Data.Text (Text)

maybeGetPort :: IO (Maybe Int)
maybeGetPort = do
  maybeRawPort <- lookupEnv "PORT"
  case maybeRawPort of
    Nothing -> return Nothing
    Just rawPort -> return (readMaybe rawPort :: Maybe Int)

getPort :: IO Int
getPort = do
  maybePort <- maybeGetPort
  case maybePort of
    Nothing -> return 8015
    Just x -> return x

getConfig :: IO Config
getConfig = do
  psqlConnection <- connectPostgreSQL ""
  return $ Config psqlConnection

api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = enter (makeHandler config) $ videoServer :<|> videoLibraryServer

makeHandler :: Config -> ConfigM :~> Handler
makeHandler config = NT (\m -> runReaderT (runConfigM m) config)

app :: Config -> Application
app config = serve api (server config)

myOptions = defaultSchemaOptions{unwrapUnaryRecords=True}

instance ToSchema VideoFile
instance ToSchema VideoFileId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema (Video VideoId)
instance ToSchema (Video NoId) where
  declareNamedSchema _ = do
    nameSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideo") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [ ("name", nameSchema) ]
      & required .~ [ "name" ]
instance ToSchema VideoId
instance ToSchema VideoLibrary
instance ToSchema VideoLibraryId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema VideoFileStorageId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToParamSchema VideoId
instance ToParamSchema VideoLibraryId

main :: IO ()
main = do
  BL8.writeFile "swagger.json" $ encode $ toSwagger (Proxy :: Proxy API)
  -- Connection info gets passed via environment variables
  config <- getConfig
  port <- getPort
  run port (logStdoutDev (app config))
  return ()
