{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Configuration where

import Data.Pool
import Database.PostgreSQL.Simple
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Models

type ConnectionPool = Pool Connection

data Configuration = Configuration {
    configurationFile :: !ConfigurationFile
  , configurationPool :: !ConnectionPool
}

data ConfigurationFile = ConfigurationFile {
    databasePassword :: !DatabasePassword
  , databaseUsername :: !DatabaseUsername
  , databaseName     :: !DatabaseName
} deriving (Eq, Generic)

instance FromJSON ConfigurationFile
instance ToJSON ConfigurationFile

type ConfigIO = ReaderT Configuration IO

