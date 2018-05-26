{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Lens
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.HashMap.Strict.InsOrd
import Data.Proxy (Proxy(..))
import Data.Swagger
import Data.Text (Text)
import Servant.API
import Servant.Swagger

import Crik.API
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

myOptions = defaultSchemaOptions{unwrapUnaryRecords=True}

instance ToSchema VideoFileId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema VideoFileStorageId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema VideoId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema VideoLibraryId where declareNamedSchema = genericDeclareNamedSchema myOptions

instance ToSchema (VideoFile NoId) where
  declareNamedSchema _ = do
    videoIdSchema <- declareSchemaRef (Proxy :: Proxy VideoId)
    videoLibraryIdSchema <- declareSchemaRef (Proxy :: Proxy VideoLibraryId)
    videoFileStorageIdSchema <- declareSchemaRef (Proxy :: Proxy VideoFileStorageId)
    videoFileUrlSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideoFile") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [
        ("videoId", videoIdSchema)
      , ("videoLibraryId", videoLibraryIdSchema)
      , ("videoFileStorageId", videoFileStorageIdSchema)
      , ("videoFileUrl", videoFileUrlSchema)
      ]
      & required .~ ["id", "videoId", "videoLibraryId", "videoFileStorageId", "videoFileUrl"]

instance ToSchema (VideoFile VideoFileId) where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef (Proxy :: Proxy VideoFileId)
    return $ NamedSchema (Just "VideoFile") $ toSchema (Proxy :: Proxy (VideoFile NoId))
      & type_ .~ SwaggerObject
      & properties %~ (union [("id", idSchema)])
      & required %~ (++ ["id"])

instance ToSchema (Video VideoId) where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef (Proxy :: Proxy VideoId)
    return $ NamedSchema (Just "Video") $ toSchema (Proxy :: Proxy (Video NoId))
      & type_ .~ SwaggerObject
      & properties %~ (union [("id", idSchema)])
      & required %~ (++ ["id"])

instance ToSchema (Video NoId) where
  declareNamedSchema _ = do
    nameSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideo") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [("name", nameSchema)]
      & required .~ ["name"]

instance ToSchema (Video (Maybe VideoId)) where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef (Proxy :: Proxy VideoId)
    nameSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "VideoOptionalId") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [("name", nameSchema), ("id", idSchema)]
      & required .~ ["name"]

instance ToSchema (VideoLibrary VideoLibraryId) where
  declareNamedSchema _ = do
    videoLibraryIdSchema <- declareSchemaRef (Proxy :: Proxy VideoLibraryId)
    return $ NamedSchema (Just "VideoLibrary") $ toSchema (Proxy :: Proxy (VideoLibrary NoId))
      & type_ .~ SwaggerObject
      & properties %~ (union [("id", videoLibraryIdSchema)])
      & required %~ (++ ["id"])

instance ToSchema (VideoLibrary NoId) where
  declareNamedSchema _ = do
    videoLibraryUrlSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideoLibrary") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [("videoLibraryUrl", videoLibraryUrlSchema)]
      & required .~ ["videoLibraryUrl"]

instance ToParamSchema VideoId
instance ToParamSchema VideoFileId
instance ToParamSchema VideoLibraryId

apiSwagger :: Swagger
apiSwagger = toSwagger (Proxy :: Proxy CrikAPI)

addResponseCode statusCode statusDescription subAPI fullAPI swagger =
  setResponseFor (subOperations subAPI fullAPI) statusCode (return responseSchema) swagger
  where responseSchema = (mempty :: Response) & description .~ statusDescription

addAllVideoLibraryFilesResponses :: Swagger -> Swagger
addAllVideoLibraryFilesResponses =
  addResponseCode 422 "Invalid video library path" (Proxy :: Proxy GetAllFilesInVideoLibrary)
    (Proxy :: Proxy CrikAPI)

addNewVideoLibraryFilesResponses :: Swagger -> Swagger
addNewVideoLibraryFilesResponses =
  addResponseCode 422 "Invalid video library path" (Proxy :: Proxy GetNewFilesInVideoLibrary)
    (Proxy :: Proxy CrikAPI)

main :: IO ()
main = do
  let swagger = (addNewVideoLibraryFilesResponses . addAllVideoLibraryFilesResponses) apiSwagger
  BL8.writeFile "swagger.json" $ encodePretty swagger
