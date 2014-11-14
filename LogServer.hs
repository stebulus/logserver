{-# LANGUAGE OverloadedStrings #-}
import Codec.MIME.Type (Type(..), MIMEType(..), MIMEParam(..))
import Codec.MIME.Parse (parseContentType)
import Control.Concurrent.MVar (newMVar, MVar, withMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT)
import Data.ByteString (ByteString, hPut)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.CaseInsensitive as CI
import Data.Encoding (encodingFromStringExplicit, decodeLazyByteStringExplicit)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid (mconcat)
import Data.Text (Text, pack, unpack, toLower)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.IO (hPutStr)
import Data.Version (showVersion)
import Network.Wai (Application, responseLBS, requestMethod, requestBody,
    requestHeaders, Request, Response)
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
app log req respond = do
    e <- runEitherT $ do
        when (requestMethod req /= methodPost)
             $ left $ responseLBS
                 status405  -- Method Not Allowed
                 [("Allow", "POST"), ("Content-Type", "text/plain")]
                 "Only POST to this server.\r\n"
        txt <- getText (lookup (CI.mk "Content-Type") (requestHeaders req))
                       (requestBody req)
        liftIO $ withMVar log $ \h -> do
            hPutStr h txt
            hFlush h
            return $ responseLBS
                status200  -- OK
                [("Content-Type", "text/plain")]
                "Logged.\r\n"
    either respond respond e

getText :: Maybe ByteString -> IO ByteString -> EitherT Response IO Text
getText maybeContentType input = do
    case fmap mimeType contentType of
        Nothing -> left $ responseLBS
                        status400  -- Bad Request
                        [("Content-Type", "text/plain")]
                        $ mconcat [ "Incomprehensible Content-Type: "
                                  , contentTypeBS
                                  , "\r\n" ]
        Just (Text _) -> return ()
        _ -> left $ responseLBS
                status415  -- Unsupported Media Type
                [("Content-Type", "text/plain")]
                $ mconcat [ "Submit text/* to this server, not "
                          , contentTypeBS
                          , "\r\n" ]
    enc <- maybe (left $ responseLBS
                        status400  -- Bad Request
                        [("Content-Type", "text/plain; charset=utf-8")]
                        $ mconcat [ "Unknown charset "
                                  , fromStrict $ encodeUtf8 charset
                                  , "\r\n" ])
                 right
                 $ encodingFromStringExplicit $ unpack charset
    bss <- liftIO $ sequenceWhile (return . (/= ""))
                                  (repeat $ fmap fromStrict input)
    case decodeLazyByteStringExplicit enc $ mconcat bss of
        Left e -> left $ responseLBS
                    status400  -- Bad Request
                    [("Content-Type", "text/plain; charset=utf-8")]
                    $ mconcat [ "Character encoding error: "
                              , fromStrict $ encodeUtf8 $ pack $ show e
                              , "\r\n" ]
        Right txt -> return $ pack txt
    where contentTypeBSS = -- default per RFC 2616 section 7.2.1
                           fromMaybe "application/octet-stream" maybeContentType
          contentTypeBS = fromStrict contentTypeBSS
          contentType = (fromEither $ decodeUtf8' contentTypeBSS) >>= parseContentType
          charset = -- default per RFC 2616 section 3.7.1
                    fromMaybe "iso-8859-1"
                    $ lookup "charset"
                    . map (\x -> (toLower (paramName x), paramValue x))
                    . mimeParams
                    =<< contentType

sequenceWhile :: (Monad m) => (a -> m Bool) -> [m a] -> m [a]
sequenceWhile pred = loop
    where loop [] = return []
          loop (ma:mas) = do
                a <- ma
                good <- pred a
                if good
                  then do rest <- loop mas
                          return (a:rest)
                  else return []

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
