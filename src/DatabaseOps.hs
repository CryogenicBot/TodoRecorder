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

import Models
import Configuration

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

initConnectionPool :: ConfigurationFile -> IO ConnectionPool
initConnectionPool (ConfigurationFile (DatabasePassword dbPass) (DatabaseUsername dbUser) (DatabaseName dbName)) = do
  let connectInfo = defaultConnectInfo { connectUser =  dbUser, connectPassword = dbPass, connectDatabase = dbName}
  createPool (connect connectInfo)
              close
              2 -- stripes
              60 -- unused connections are kept open for a minute
              10 -- max. 10 connections open per stripe

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