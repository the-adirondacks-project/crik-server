{-# LANGUAGE DeriveGeneric #-}

module Types
(
  NoId(..)
) where

import GHC.Generics (Generic)

data NoId = NoId deriving (Show, Generic)
