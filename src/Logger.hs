{-# LANGUAGE OverloadedStrings #-}
module Logger where

import System.Clock

data LogLevel = NONE | FATAL | ERROR | INFO | VERBOSE | DEBUG deriving (Show, Eq, Ord)

data LogMessage = LogMessage { message :: String
                             , moduleName :: String
                             , logLevel :: LogLevel
                             , timestamp :: TimeSpec
                         } deriving Show


createNewLogMessage :: LogLevel -> String -> String -> IO LogMessage
createNewLogMessage ll msg modName = do
    time <- getTime Realtime
    return LogMessage {message = msg, moduleName = modName, logLevel = ll, timestamp = time}

logMessageMod ll msg modName = do
    msg <- createNewLogMessage ll msg modName
    putStrLn $ show msg

logMessage ll msg = logMessageMod ll msg "default"

logDefault msg = logMessage INFO msg
