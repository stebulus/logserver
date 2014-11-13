{-# LANGUAGE OverloadedStrings #-}
import Codec.MIME.Type (Type(..), MIMEType(..))
import Codec.MIME.Parse (parseContentType)
import Control.Concurrent.MVar (newMVar, MVar, withMVar)
import Control.Monad (when)
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT)
import Data.ByteString (empty, hPut)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.CaseInsensitive as CI
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid (mconcat)
import Data.Text.Encoding (decodeUtf8')
import Data.Version (showVersion)
import Network.Wai (Application, responseLBS, requestMethod, requestBody,
    requestHeaders, Request)
import Network.HTTP.Types (status200, status400, status415, status405,
    methodPost)
import Network.Wai.Handler.Warp
    (runSettings, setHost, setPort, defaultSettings, Port)
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
                runSettings (setHost "127.0.0.1" $ setPort port defaultSettings)
                            (app mh)

app :: MVar Handle -> Application
app log req respond =
    either id id =<< (runEitherT $ do
        when (requestMethod req /= methodPost) $ bad
             status405  -- Method Not Allowed
             [("Allow", "POST"), ("Content-Type", "text/plain")]
             "Only POST to this server.\r\n"
        case fmap mimeType contentType of
            Nothing -> bad status400  -- Bad Request
                           [("Content-Type", "text/plain")]
                           $ mconcat [ "Incomprehensible Content-Type: "
                                     , contentTypeBS
                                     , "\r\n" ]
            Just (Text _) -> continue
            _ -> bad status415  -- Unsupported Media Type
                     [("Content-Type", "text/plain")]
                     $ mconcat [ "Submit text/* to this server, not "
                               , contentTypeBS
                               , "\r\n" ]
        return $ withMVar log $ \h -> do
            while (/= empty)
                  (requestBody req)
                  (hPut h)
            hFlush h
            respond $ responseLBS
                status200  -- OK
                [("Content-Type", "text/plain")]
                "Logged.\r\n"
    where bad status headers body =
            left $ respond $ responseLBS status headers body
          continue = return ()
          contentTypeBSS = fromMaybe "application/octet-stream"
                           $ lookup (CI.mk "Content-Type") (requestHeaders req)
                           -- default content-type is application/octet-stream
                           -- per RFC 2616 section 7.2.1
          contentTypeBS = fromStrict contentTypeBSS
          contentType = (fromEither $ decodeUtf8' contentTypeBSS) >>= parseContentType

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

fromEither :: Either a b -> Maybe b
fromEither (Left _) = Nothing
fromEither (Right b) = Just b

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
