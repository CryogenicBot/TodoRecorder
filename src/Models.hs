{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Models where  

-- TODO: user type aliases where possible
import Data.Text (Text(..), pack, unpack)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.LocalTime
import GHC.Generics

-- TODO: move this to DatabaseOps after getUser endpoint is removed
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
    currentUserId :: Int
  , recordType    :: Text
  , apiKey        :: Text
} deriving (Eq, Show, Generic)

instance ToJSON RecordCreateRequest
instance FromJSON RecordCreateRequest

data Record = Record {
    recordApiKey            :: Text
  , timeSinceRecordCreated  :: LocalTime
} deriving (Eq, Show, Generic)

instance ToJSON Record
instance FromJSON Record

type RecordType = Text

data Records = Records {
    movieRecords :: Maybe [Record]
  , bookRecords  :: Maybe [Record]
  , gameRecords  :: Maybe [Record]
} deriving (Eq, Show, Generic)

instance ToJSON Records
instance FromJSON Records

newtype DatabaseName     = DatabaseName String deriving (Eq, Generic)
newtype DatabaseUsername = DatabaseUsername String deriving (Eq, Generic)
newtype DatabasePassword = DatabasePassword String deriving (Eq, Generic)

instance ToJSON DatabaseName
instance ToJSON DatabaseUsername
instance ToJSON DatabasePassword

instance FromJSON DatabaseName
instance FromJSON DatabaseUsername
instance FromJSON DatabasePassword