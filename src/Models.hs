{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Models where  

import Data.Text (Text(..), pack, unpack)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

data User = User { 
    userId        :: Int
  , userEmail     :: Text
  , userPassword  :: Text
} deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data UserCreateRequest = UserCreateRequest { 
  email     :: Text
, password  :: Text
} deriving (Eq, Show, Generic)

instance ToJSON UserCreateRequest
instance FromJSON UserCreateRequest

data RecordCreateRequest = RecordCreateRequest { 
  currentUserId       :: Int
, recordType  :: Text
, apiKey              :: Text
} deriving (Eq, Show, Generic)

instance ToJSON RecordCreateRequest
instance FromJSON RecordCreateRequest

data Record = Record {
  recordId      :: Int
, recordApiKey  :: Text
}

type RecordType = Text

newtype DatabaseName     = DatabaseName String deriving (Eq, Generic)
newtype DatabaseUsername = DatabaseUsername String deriving (Eq, Generic)
newtype DatabasePassword = DatabasePassword String deriving (Eq, Generic)

instance ToJSON DatabaseName
instance ToJSON DatabaseUsername
instance ToJSON DatabasePassword

instance FromJSON DatabaseName
instance FromJSON DatabaseUsername
instance FromJSON DatabasePassword