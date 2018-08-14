-- set up connection pool
-- set up simple get users query
{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module DatabaseOps where

import Data.Int(Int64)
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.Reader
import Data.Text (Text(..), pack, unpack)

import Models
import Configuration

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Record where
  fromRow = Record <$> field <*> field

-- TODO: check Data.Pool documentation and change the following settings
-- TODO: add settings to config file and get it from there
initConnectionPool :: ConfigurationFile -> IO ConnectionPool
initConnectionPool (ConfigurationFile (DatabasePassword dbPass) (DatabaseUsername dbUser) (DatabaseName dbName)) = do
  let connectInfo = defaultConnectInfo { connectUser =  dbUser, connectPassword = dbPass, connectDatabase = dbName}
  createPool (connect connectInfo)
              close
              2 -- stripes
              60 -- unused connections are kept open for a minute
              10 -- max. 10 connections open per stripe

-- TODO: Remove this after removing temporary GET /users endpoint
dBgetUsers :: ConfigIO [User]
dBgetUsers = do 
  config@(Configuration _ conns) <- ask
  liftIO $ withResource conns $ \conn ->
    query_ conn "SELECT * FROM tdrc.users"

dBcreateUser :: UserCreateRequest -> ConfigIO (Int64)
dBcreateUser user@(UserCreateRequest email pass) = do 
  config@(Configuration _ conns) <- ask
  liftIO $ withResource conns $ \conn ->
    execute conn "INSERT INTO tdrc.users (email, password) values (?, ?)"
                  (email, pass)

dBcreateRecord :: RecordCreateRequest -> ConfigIO (Int64)
dBcreateRecord req@(RecordCreateRequest id rType apiKey) = do
  let createRecordQuery = getCreateRecordQuery req
  config@(Configuration _ conns) <- ask
  liftIO $ withResource conns $ \conn -> do
    [Only recordId] <- query conn createRecordQuery (Only apiKey)
    execute conn (getCreateRecordLinkQuery req) (id::Int, recordId::Int)

dBretrieveRecordForUser :: RecordType -> Int -> ConfigIO [Record]
dBretrieveRecordForUser rType userId = do
  let retrieveQuery = getRetrieveRecordForUserQuery rType
  config@(Configuration _ conns) <- ask
  liftIO $ withResource conns $ \conn -> do
    query conn retrieveQuery (Only userId)

-- TODO: simplify following three functions and add better error handling
getCreateRecordQuery :: RecordCreateRequest -> Query
getCreateRecordQuery (RecordCreateRequest _ "movie" _)  = "INSERT INTO tdrc.movie_records (api_id) VALUES (?) ON CONFLICT (api_id) DO UPDATE SET api_id = EXCLUDED.api_id RETURNING movie_id"
getCreateRecordQuery (RecordCreateRequest _ "game" _)   = "INSERT INTO tdrc.game_records (api_id) VALUES (?) ON CONFLICT (api_id) DO UPDATE SET api_id = EXCLUDED.api_id RETURNING game_id"
getCreateRecordQuery (RecordCreateRequest _ "book" _)   = "INSERT INTO tdrc.book_records (api_id) VALUES (?) ON CONFLICT (api_id) DO UPDATE SET api_id = EXCLUDED.api_id RETURNING book_id"

getCreateRecordLinkQuery :: RecordCreateRequest -> Query
getCreateRecordLinkQuery (RecordCreateRequest _ "movie" _)  = "INSERT INTO tdrc.user_movie_records (user_id, movie_id) VALUES (?, ?)"
getCreateRecordLinkQuery (RecordCreateRequest _ "game" _)   = "INSERT INTO tdrc.user_game_records (user_id, game_id) VALUES (?, ?)"
getCreateRecordLinkQuery (RecordCreateRequest _ "book" _)   = "INSERT INTO tdrc.user_book_records (user_id, book_id) VALUES (?, ?)"

getRetrieveRecordForUserQuery :: RecordType -> Query
getRetrieveRecordForUserQuery "movie" = "SELECT api_id, create_time FROM tdrc.user_movie_records INNER JOIN tdrc.movie_records ON tdrc.user_movie_records.movie_id = tdrc.movie_records.movie_id WHERE user_id = ?"
getRetrieveRecordForUserQuery "game"  = "SELECT api_id, create_time FROM tdrc.user_game_records INNER JOIN tdrc.game_records ON tdrc.user_game_records.game_id = tdrc.game_records.game_id WHERE user_id = ?"
getRetrieveRecordForUserQuery "book"  = "SELECT api_id, create_time FROM tdrc.user_book_records INNER JOIN tdrc.book_records ON tdrc.user_book_records.book_id = tdrc.book_records.book_id WHERE user_id = ?"

-- TODO change Text to alias for Email
checkIfUserExists :: Text -> ConfigIO Bool
checkIfUserExists email = do
  config@(Configuration _ conns) <- ask
  liftIO $ withResource conns $ \conn -> do
    [Only answer] <- query conn "SELECT EXISTS(SELECT * FROM tdrc.users where email = ?)" (Only email)
    return answer
