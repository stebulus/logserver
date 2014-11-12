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
import System.IO (stderr, hPutStrLn, withFile, IOMode(AppendMode), Handle)

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

main = do
    args <- getArgs
    if length args /= 2
      then do
        name <- getProgName
        hPutStrLn stderr $ "usage: " ++ name ++ " port filename"
        exitWith $ ExitFailure 2
      else do
        case maybeRead (args!!0) of
          Nothing -> do
            hPutStrLn stderr $
                "error: cannot interpret " ++ args!!0 ++ " as an integer"
            exitWith $ ExitFailure 2
          Just port -> do
            putStrLn $ "binding to port " ++ show port
            withFile (args!!1) AppendMode $ \h -> do
                putStrLn $ "logging to file " ++ (args!!1)
                mh <- newMVar h
                run port (app mh)
