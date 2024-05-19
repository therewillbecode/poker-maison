{-# LANGUAGE OverloadedStrings #-}

module Env where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.UTF8 as BSU (fromString)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Text (pack)
import Database.Redis
  ( ConnectInfo,
    Redis,
    connect,
    connectHost,
    connectPort,
    defaultConnectInfo,
    parseConnectInfo,
    runRedis,
    setex,
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
--import Web.JWT (secret)
import Prelude

getRedisHostFromEnv :: String -> IO ConnectInfo
getRedisHostFromEnv defaultHostName = do
  maybeConnInfo <- lookupEnv "redisHost"
  case maybeConnInfo of
    Nothing -> do
      print "couldn't parse redishost from env default used"
      return defaultRedisConn
    Just hostname -> do
      print "Redis host name from env is: "
      print hostname
      return $ defaultConnectInfo {connectHost = hostname}
  where
    defaultRedisConn = defaultConnectInfo {connectHost = defaultHostName}

-- get the postgres connection string from dbConnStr env variable
getDBConnStrFromEnv :: IO C.ByteString
getDBConnStrFromEnv = do
  dbConnStr <- lookupEnv "dbConnStr"
  case dbConnStr of
    Nothing -> error "Missing dbConnStr in env"
    Just conn -> return $ C.pack conn

-- get the port from the userAPIPort env variable
getAuthAPIPort :: Int -> IO Int
getAuthAPIPort defaultPort = do
  maybeEnvPort <- lookupEnv "authAPIPort"
  case maybeEnvPort of
    Nothing -> return defaultPort
    Just port -> maybe (return defaultPort) return (readMaybe port)

-- get the port from the socketAPIPort env variable
getSocketAPIPort :: Int -> IO Int
getSocketAPIPort defaultPort = do
  maybeEnvPort <- lookupEnv "socketPort"
  case maybeEnvPort of
    Nothing -> return defaultPort
    Just port -> maybe (return defaultPort) return (readMaybe port)

-- get the secret key for signing JWT authentication tokens
getSecretKey :: IO C.ByteString
getSecretKey = do
  maybeSecretKey <- lookupEnv "secret"
  case maybeSecretKey of
    Nothing -> error "Missing secret key in env"
    Just s -> return $ BSU.fromString s
