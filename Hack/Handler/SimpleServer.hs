{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------
-- |
-- Module        : Hack.Handler.SimpleServer
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A simplistic HTTP server handler for Hack.
--
---------------------------------------------------------
module Hack.Handler.SimpleServer
    ( run
    ) where

import Hack
import qualified System.IO

import Web.Encodings.StringLike (takeUntilBlank)
import Web.Encodings.MimeHeader (parseHeader, lookupHeader)

import qualified Data.ByteString.Lazy as BL
import Network
    ( listenOn, accept, sClose, PortID(PortNumber), Socket
    , withSocketsDo)
import Control.Exception (bracket, finally, Exception)
import System.IO (Handle, hClose)
import Control.Concurrent
import Control.Monad (unless)
import Data.Maybe (isJust, fromJust)

import Control.Failure
import Data.Typeable (Typeable)

import Web.Encodings.StringLike (StringLike)
import qualified Web.Encodings.StringLike as SL

run :: Port -> Application -> IO ()
run port = withSocketsDo .
    bracket
        (listenOn $ PortNumber $ fromIntegral port)
        sClose .
        serveConnections port
type Port = Int

serveConnections :: Port -> Application -> Socket -> IO ()
serveConnections port app socket = do
    (conn, remoteHost', _) <- accept socket
    forkIO $ serveConnection port app conn remoteHost'
    serveConnections port app socket

serveConnection :: Port -> Application -> Handle -> String -> IO ()
serveConnection port app conn remoteHost' =
    finally
        serveConnection'
        (hClose conn)
    where
        serveConnection' = do
            env <- hParseEnv port conn remoteHost'
            res <- app env
            sendResponse conn res

hParseEnv :: Port -> Handle -> String -> IO Env
hParseEnv port conn remoteHost' = do
    content' <- BL.hGetContents conn
    let (headers', body') = takeUntilBlank content'
    parseEnv port headers' body' remoteHost'

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d

data InvalidRequest =
    NotEnoughLines [String]
    | HostNotIncluded
    | BadFirstLine String
    | NonHttp11
    deriving (Show, Typeable)
instance Exception InvalidRequest

-- | Parse a set of header lines and body into a 'Env'.
parseEnv :: (MonadFailure InvalidRequest m)
         => Port
         -> [BL.ByteString]
         -> BL.ByteString
         -> String
         -> m Env
parseEnv port lines' body' remoteHost' = do
    case lines' of
        (_:_:_) -> return ()
        _ -> failure $ NotEnoughLines $ map SL.unpack lines'
    (method', rpath', gets) <- parseFirst $ head lines'
    let method = safeRead GET (SL.unpack method')
    let rpath = '/' : case SL.unpack rpath' of
                        ('/':x) -> x
                        _ -> SL.unpack rpath'
    let heads = map parseHeaderNoAttr $ tail lines'
        heads' = map (\(x, y) -> (SL.unpack x, SL.unpack y)) heads
    let host' = lookup (SL.pack "Host") heads
    unless (isJust host') $ failure HostNotIncluded
    let host = fromJust host'
    let len = maybe "0" SL.unpack
            $ lookup (SL.pack "Content-Length") heads
    let body'' = BL.take (safeRead 0 len) body'
    let (serverName', _) = SL.breakChar ':' host
    return $ Env
                { requestMethod = method
                , scriptName = ""
                , pathInfo = rpath
                , queryString = SL.unpack gets
                , serverName = SL.unpack serverName'
                , serverPort = port
                , http = heads'
                , hackVersion = [2009, 10, 30]
                , hackUrlScheme = HTTP
                , hackInput = body''
                , hackErrors = System.IO.hPutStr System.IO.stderr
                , hackHeaders = []
                , hackCache = []
                , remoteHost = remoteHost'
                }

parseFirst :: (StringLike s, MonadFailure InvalidRequest m) =>
              s
           -> m (s, s, s)
parseFirst s = do
    let pieces = SL.split ' ' s
    (method, query, http') <-
        case pieces of
            [x, y, z] -> return (x, y, z)
            _ -> failure $ BadFirstLine $ SL.unpack s
    unless (http' == SL.pack "HTTP/1.1") $ failure NonHttp11
    let (rpath, qstring) = SL.breakChar '?' query
    return (method, rpath, qstring)

sendResponse :: Handle -> Response -> IO ()
sendResponse h res = do
    BL.hPut h $ SL.pack "HTTP/1.1 "
    BL.hPut h $ SL.pack $ show $ status res
    BL.hPut h $ SL.pack "\r\n"
    mapM_ putHeader $ headers res
    BL.hPut h $ SL.pack "\r\n"
    BL.hPut h $ body res
    where
        putHeader (x, y) = do
            BL.hPut h $ SL.pack x
            BL.hPut h $ SL.pack ": "
            BL.hPut h $ SL.pack y
            BL.hPut h $ SL.pack "\r\n"
{-
    hPutStr conn $ "HTTP/1.1 " ++ code res ++ "\r\n"
    hPutStr conn $ "Content-type: " ++ contentType res ++ "\r\n"
    let headers' = map (\(x, y) -> x ++ ": " ++ y ++ "\r\n") $ headers res
    hPutStr conn $ concat headers'
    hPutStr conn "\r\n"
    BL.hPutStr conn $ content res
-}

parseHeaderNoAttr :: StringLike a => a -> (a, a)
parseHeaderNoAttr s =
    let (k, rest) = SL.span (/= ':') s
     in (k, SL.dropPrefix' (SL.pack ": ") rest)
