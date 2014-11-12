{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, MVar)
import Control.Exception (bracket)
import Data.ByteString (empty, hPut)
import Data.Maybe (listToMaybe)
import Data.Version (showVersion)
import Network.Wai
    (Application, responseLBS, requestMethod, requestBody, Request)
import Network.HTTP.Types (status200, status405, methodPost)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO
    (stderr, hPutStrLn, withFile, IOMode(AppendMode), Handle, hFlush, FilePath)

import Paths_logserver (version)

main = do
    args <- getArgs
    progname <- getProgName
    case parseArgs progname args of
        Left err -> do
            hPutStrLn stderr err
            exitWith $ ExitFailure 2
        Right ShowVersion ->
            putStrLn $ "logserver-" ++ (showVersion version)
        Right (RunServer port filename) ->
            withFile filename AppendMode $ \h -> do
                mh <- newMVar h
                run port (app mh)

app :: MVar Handle -> Application
app log req respond =
    if requestMethod req == methodPost
      then
        bracket (takeMVar log)
                (putMVar log)
                (\h -> do while (/= empty)
                                (requestBody req)
                                (hPut h)
                          hFlush h
                          respond $ responseLBS
                            status200  -- OK
                            [("Content-Type", "text/plain")]
                            "Logged.\r\n")
      else
        respond $ responseLBS
            status405  -- Method Not Allowed
            [("Allow", "POST"), ("Content-Type", "text/plain")]
            "Only POST to this server.\r\n"

while :: (a->Bool) -> IO a -> (a->IO ()) -> IO ()
while pred io act = loop
    where loop = do
            a <- io
            if pred a
              then do act a
                      loop
              else return ()

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = do
    (a,unparsed) <- listToMaybe $ reads s
    if unparsed == "" then Just a else Nothing

usage :: String -> String
usage progname = "usage: " ++ progname ++ " port filename\n\
                 \       " ++ progname ++ " --version"

data Action = RunServer Port FilePath
            | ShowVersion

parseArgs :: String -> [String] -> Either String Action
parseArgs _ [verflag]
    | verflag == "--version" = Right ShowVersion
parseArgs _ [port,filename] =
    case maybeRead port of
        Nothing ->
            Left $ "error: cannot interpret " ++ port ++ " as an integer"
        Just n ->
            Right $ RunServer n filename
parseArgs progname _ = Left $ usage progname
