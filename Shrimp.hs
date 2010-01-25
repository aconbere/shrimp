module Shrimp where

import Hack hiding (Response, body, headers, status, requestMethod)
import Network
import System.IO
import Control.Concurrent
import Data.List
import Data.Char (isSpace)

data Environment = Environment
    { serverName     :: String
    , serverPort     :: Int
    , method  :: RequestMethod
    , scriptName     :: String
    , queryString    :: String
    , headers        :: [Header]
    } deriving (Show)

data RequestLine = RequestLine
    { requestMethod :: RequestMethod
    , requestPath :: String
    , requestVersion :: String
    } deriving (Show)

type Header = (String, String)

data Response = Response
    { responseStatus   :: Int
    , resonseBody     :: String
    , responseHeaders  :: [Header]
    } deriving (Show)

statusCodes =
  [ (100, "Continue")
  , (101, "Switching Protocols")
  , (200, "OK")
  , (201, "Created")
  , (202, "Accepted")
  , (203, "Non-Authoritative Information")
  , (204, "No Content")
  , (205, "Reset Content")
  , (206, "Partial Content")
  , (300, "Multiple Choices")
  , (301, "Moved Permanently")
  , (302, "Found")
  , (303, "See Other")
  , (304, "Not Modified")
  , (305, "Use Proxy")
  , (307, "Temporary Redirect")
  , (400, "Bad Request")
  , (401, "Unauthorized")
  , (402, "Payment Required")
  , (403, "Forbidden")
  , (404, "Not Found")
  , (405, "Method Not Allowed")
  , (406, "Not Acceptable")
  , (407, "Proxy Authentication Required")
  , (408, "Request Timeout")
  , (409, "Conflict")
  , (410, "Gone")
  , (411, "Length Required")
  , (412, "Precondition Failed")
  , (413, "Request Entity Too Large")
  , (414, "Request-URI Too Large")
  , (415, "Unsupported Media Type")
  , (416, "Requested Range Not Satisfiable")
  , (417, "Expectation Failed")
  , (500, "Internal Server Error")
  , (501, "Not Implemented")
  , (502, "Bad Gateway")
  , (503, "Service Unavailable")
  , (504, "Gateway Timeout")
  , (505, "HTTP Version Not Supported")
  ]

doLog message = putStrLn message

run host port urlMap = withSocketsDo $ do
    doLog $ "Starting Shrimp on " ++ host ++ ":" ++ (show port)
    socket <- listenOn (PortNumber $ fromIntegral port)
    serveConnections host port socket urlMap

serveConnections host port socket urlMap = do
    (handle, _remoteHost, _remotePort) <- accept socket
    hSetNewlineMode handle (NewlineMode CRLF CRLF)
    hSetBuffering handle NoBuffering
    forkIO $ serveConnection host port handle urlMap
    serveConnections host port socket urlMap

serveConnection host port handle urlMap = do
    top <- removeHead handle
    let request = parseRequestLine (top!!0)
    let headers = parseHeaders $ tail top
    let env = Environment host port (requestMethod request) "" "" headers

    case lookup (requestPath request) urlMap of
        Just publisher -> 
            hPutStrLn handle $ buildResponse (publisher env)
        Nothing ->
            hPutStrLn handle $ buildResponse (Response 404 "Not Found" defaultHeaders)
    closeConnection handle

removeHead handle = removeHead' handle []
removeHead' handle h = do
    t <- hGetLine handle
    if t == ""
        then (return h)
        else (removeHead' handle (h ++ [t]))

parseRequestLine line =
    RequestLine (read (bits!!0)::RequestMethod) (bits!!1) (bits!!2)
    where bits = words line

parseHeaders = map (parseHeader)
parseHeader line = (is!!0, is!!1)
    where is = (map trim) (split line ':')

trim = f . f
    where f = reverse . dropWhile isSpace

split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

defaultHeaders = [ ("Content-Type", "text/html")
                 , ("Server", "Shrimp 0.1 a tiny web server") ]

renderHeader (key, value) = key ++ ": " ++ value
renderHeaders headers = concat $ intersperse "\n" $ map renderHeader headers

buildResponse resp =
    concat $ intersperse "\n" [ "HTTP/1.1 " ++ (show _status) ++ _reason
                              , renderHeaders (_headers ++ [content_length])
                              , ""
                              , _body]
    where Response _status _body _headers = resp
          _reason = case lookup _status statusCodes of
                Just r -> r
                _ -> ""
          content_length = ("Content-Length", (show $ length _body))

closeConnection handle = hClose handle
