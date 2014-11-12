{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, MVar)
import Control.Exception (bracket)
import Data.ByteString (hPut)
import Data.Maybe (listToMaybe)
import Network.Wai
    (Application, responseLBS, requestMethod, requestBody, Request)
import Network.HTTP.Types (status200, status405, methodPost)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO
    (stderr, hPutStrLn, withFile, IOMode(AppendMode), Handle, hFlush)

copyChunk :: Request -> Handle -> IO Bool
copyChunk req h = do
    chunk <- requestBody req
    if chunk == ""
      then return False
      else do
        hPut h chunk
        return True

copyAll :: Request -> Handle -> IO ()
copyAll req h = do
    result <- copyChunk req h
    if result
      then copyAll req h
      else return ()

app :: MVar Handle -> Application
app log req respond =
    if requestMethod req == methodPost
      then
        bracket (takeMVar log)
                (\h -> putMVar log h)
                (\h -> do copyAll req h
                          hFlush h
                          respond $ responseLBS
                            status200
                            [("Content-Type", "text/plain")]
                            "Logged.")
      else
        respond $ responseLBS
            status405
            [("Allow", "POST"), ("Content-Type", "text/plain")]
            "Only POST to this server."

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = do
    (a,xs) <- listToMaybe $ reads s
    case xs of
        "" -> Just a
        otherwise -> Nothing

usage :: String -> String
usage progname = "usage: " ++ progname ++ " port filename"

parseArgs :: String -> [String] -> Either String (Int,String)
parseArgs _ [port,filename] =
    case maybeRead port :: Maybe Int of
        Nothing ->
            Left $ "error: cannot interpret " ++ port ++ " as an integer"
        Just n ->
            Right (n, filename)
parseArgs progname _ = Left $ usage progname

main = do
    args <- getArgs
    progname <- getProgName
    case parseArgs progname args of
        Left err -> do
            hPutStrLn stderr err
            exitWith $ ExitFailure 2
        Right (port, filename) -> do
            withFile filename AppendMode $ \h -> do
                mh <- newMVar h
                run port (app mh)
