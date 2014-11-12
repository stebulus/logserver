{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (listToMaybe)
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr, hPutStrLn)

app :: Application
app _ respond = respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, World!"

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = do
    (a,xs) <- listToMaybe $ reads s
    case xs of
        "" -> Just a
        otherwise -> Nothing

main = do
    args <- getArgs
    if length args /= 1
      then do
        name <- getProgName
        hPutStrLn stderr $ "usage: " ++ name ++ " port"
        exitWith $ ExitFailure 2
      else do
        case maybeRead (args!!0) of
          Nothing -> do
            hPutStrLn stderr $
                "error: cannot interpret " ++ args!!0 ++ " as an integer"
            exitWith $ ExitFailure 2
          Just port -> do
            putStrLn $ "binding to port " ++ show port
            run port app
