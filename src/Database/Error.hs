module Database.Error
(
  DatabaseException(..)
) where

import Control.Exception (Exception)

data DatabaseException =
  InsertReturnedNothing String |
  InsertReturnedMultiple String
  deriving (Show)

instance Exception DatabaseException
