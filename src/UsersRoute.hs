{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric, OverloadedStrings #-}

module UsersRoute (
    UsersApi
  , usersServer
) where

import Servant
import Control.Monad.Trans (liftIO)
import Data.Text (Text(..), pack, unpack)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Crypto.PasswordStore
import qualified Data.ByteString as BS
import Data.Text.Encoding
import Data.List

import Models
import DatabaseOps
import Configuration
import RouteUtilities

type GetUsers = "users" :> Get '[JSON] [User]
type CreateUser = "users" :> ReqBody '[JSON] UserCreateRequest :> Post '[JSON] UserCreateResponse
type LoginUser = "users" :> "login" :> ReqBody '[JSON] UserLoginRequest :> Post '[JSON] UserLoginResponse
type GetRecordForUser = "users" :> Capture "userId" Int :> "records" :> QueryParam "type" RecordType :> Get '[JSON] Records

type UsersApi = GetUsers :<|> CreateUser :<|> LoginUser :<|> GetRecordForUser

usersServer :: ServerT UsersApi ApiHandler
usersServer = getUsers :<|> createUser :<|> loginUser :<|> retrieveRecordsForUser

data UserCreateResponse = MissingField | Successful | UserAlreadyExists | Error deriving (Eq, Show, Generic)
instance ToJSON UserCreateResponse
instance FromJSON UserCreateResponse

newtype UserLoginRequest = UserLoginRequest UserCreateRequest deriving (Eq, Generic)
instance ToJSON UserLoginRequest
instance FromJSON UserLoginRequest

data UserLoginResponse = NotAUser | IncorrectPassword | Id Int deriving (Eq, Show, Generic)
instance ToJSON UserLoginResponse
instance FromJSON UserLoginResponse

-- TODO: only meant to aid in development, remove after live
getUsers :: ApiHandler [User]
getUsers = runConfigIOAction dBgetUsers

loginUser :: UserLoginRequest -> ApiHandler UserLoginResponse
loginUser userLoginReq = do
  users <- runConfigIOAction dBgetUsers
  return $ loginUserGivenUserList users userLoginReq

loginUserGivenUserList :: [User] -> UserLoginRequest -> UserLoginResponse
loginUserGivenUserList users (UserLoginRequest request) = 
  let userEnteredEmail = email request
      userEnteredPass = password request
      userFound = flip find users $ \user -> (userEmail user) == userEnteredEmail
  in case userFound of  Just userDetail -> if checkIfUserPassIsCorrect userDetail userEnteredPass then Id (userId userDetail) else IncorrectPassword
                        Nothing  -> NotAUser

-- TODO: replace Text with Type alias
checkIfUserPassIsCorrect :: User -> Text -> Bool
checkIfUserPassIsCorrect user pass = verifyPassword (encodeUtf8 pass) (encodeUtf8 (userPassword user))

createUser :: UserCreateRequest -> ApiHandler UserCreateResponse
createUser userCreateRequest = do
  userAlreadyExists <- runConfigIOAction $ checkIfUserExists (email userCreateRequest)
  if userAlreadyExists then return UserAlreadyExists
  else do
    securePass <- liftIO . makeSecurePass $ password userCreateRequest 
    runConfigIOAction . dBcreateUser $ UserCreateRequest (email userCreateRequest) (decodeUtf8 securePass)
    return Successful

-- TODO: replace Text with Type alias
makeSecurePass :: Text -> IO BS.ByteString
makeSecurePass unsecurePass = makePasswordWith pbkdf1 (encodeUtf8 unsecurePass) 20

retrieveRecordsForUser :: Int -> Maybe RecordType -> ApiHandler Records
retrieveRecordsForUser userId Nothing        = do
  mRecords <- retrieveRecordsForUserByType userId "movie"
  bRecords <- retrieveRecordsForUserByType userId "book"
  gRecords <- retrieveRecordsForUserByType userId "game"
  return $ Records (Just mRecords) (Just bRecords) (Just gRecords)
retrieveRecordsForUser userId (Just "movie") = do
  records <- retrieveRecordsForUserByType userId "movie"
  return $ Records (Just records) Nothing Nothing
retrieveRecordsForUser userId (Just "book")  = do
  records <- retrieveRecordsForUserByType userId "book"
  return $ Records Nothing (Just records) Nothing
retrieveRecordsForUser userId (Just "game")  = do
  records <- retrieveRecordsForUserByType userId "game"
  return $ Records Nothing Nothing (Just records)

retrieveRecordsForUserByType :: Int -> RecordType -> ApiHandler [Record]
retrieveRecordsForUserByType userId "movie" = runConfigIOAction $ dBretrieveRecordForUser "movie" userId
retrieveRecordsForUserByType userId "book"  = runConfigIOAction $ dBretrieveRecordForUser "book" userId
retrieveRecordsForUserByType userId "game"  = runConfigIOAction $ dBretrieveRecordForUser "game" userId