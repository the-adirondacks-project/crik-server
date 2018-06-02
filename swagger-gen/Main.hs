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
import Crik.Types.File
import Crik.Types.Library

myOptions = defaultSchemaOptions{unwrapUnaryRecords=True}

instance ToSchema FileId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema FileStorageId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema VideoId where declareNamedSchema = genericDeclareNamedSchema myOptions
instance ToSchema LibraryId where declareNamedSchema = genericDeclareNamedSchema myOptions

instance ToSchema (File NoId) where
  declareNamedSchema _ = do
    videoIdSchema <- declareSchemaRef (Proxy :: Proxy VideoId)
    videoLibraryIdSchema <- declareSchemaRef (Proxy :: Proxy LibraryId)
    videoFileStorageIdSchema <- declareSchemaRef (Proxy :: Proxy FileStorageId)
    videoFileUrlSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideoFile") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [
        ("videoId", videoIdSchema)
      , ("libraryId", videoLibraryIdSchema)
      , ("fileStorageId", videoFileStorageIdSchema)
      , ("videoFileUrl", videoFileUrlSchema)
      ]
      & required .~ ["id", "videoId", "libraryId", "fileStorageId", "videoFileUrl"]

instance ToSchema (File FileId) where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef (Proxy :: Proxy FileId)
    return $ NamedSchema (Just "File") $ toSchema (Proxy :: Proxy (File NoId))
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

instance ToSchema (Library LibraryId) where
  declareNamedSchema _ = do
    videoLibraryIdSchema <- declareSchemaRef (Proxy :: Proxy LibraryId)
    return $ NamedSchema (Just "Library") $ toSchema (Proxy :: Proxy (Library NoId))
      & type_ .~ SwaggerObject
      & properties %~ (union [("id", videoLibraryIdSchema)])
      & required %~ (++ ["id"])

instance ToSchema (Library NoId) where
  declareNamedSchema _ = do
    videoLibraryUrlSchema <- declareSchemaRef (Proxy :: Proxy Text)
    return $ NamedSchema (Just "NewVideoLibrary") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ [("videoLibraryUrl", videoLibraryUrlSchema)]
      & required .~ ["videoLibraryUrl"]

instance ToParamSchema VideoId
instance ToParamSchema FileId
instance ToParamSchema LibraryId

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
