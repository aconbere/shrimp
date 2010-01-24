module Shrimp where

import Hack hiding (Response, body, headers, status)
import Network
import System.IO
import Control.Concurrent
import Data.List

doLog message = putStrLn message

run port urlMap = withSocketsDo $ do
    socket <- listenOn (PortNumber port)
    serveConnections socket urlMap
    serveConnections socket urlMap
    serveConnections socket urlMap
    serveConnections socket urlMap

serveConnections socket urlMap = do
    (conn, rHost, _port) <- accept socket
    hSetNewlineMode conn (NewlineMode CRLF CRLF)
    hSetBuffering conn NoBuffering
    doLog $ "Connected to : " ++ rHost ++ " on: " ++ (show _port)
    forkIO $ serveConnection conn urlMap 
    serveConnections socket urlMap

serveConnection conn urlMap = do
    top <- removeHead conn
    let request = parseRequestLine (top!!0)
    --let headers = parseHeaders (init top)
    case lookup (path request) urlMap of
        Just publisher -> 
            hPutStrLn conn $ buildResponse (publisher 10)
        Nothing ->
            hPutStrLn conn $ buildResponse (Response 404 "Not Found" defaultHeaders)
    closeConnection conn

data RequestLine = RequestLine
  {  method :: RequestMethod
  , path :: String
  , version :: String
  } deriving (Show)

data Response = Response
  {  status   :: Int
  ,  body     :: String
  ,  headers  :: [(String, String)]
  } deriving (Show)

parseRequestLine top =
    RequestLine (read (bits!!0)::RequestMethod) (bits!!1) (bits!!2)
    where bits = words top

removeHead conn = removeHead' conn []
removeHead' conn h = do
    t <- hGetLine conn
    if t == ""
        then (return h)
        else (removeHead' conn (h ++ [t]))

defaultHeaders = [ ("Content-Type", "text/html")
                 , ("Server", "Shrimp 0.1 a tiny web server") ]

renderHeader (key, value) = key ++ ": " ++ value
renderHeaders headers = concat $ intersperse "\n" $ map renderHeader headers

buildResponse env =
    concat $ intersperse "\n" [ "HTTP/1.1 " ++ (show _status) ++ "OK"
                              , renderHeaders (_headers ++ [content_length])
                              , ""
                              , _body]
    where _status = status env
          _headers = headers env
          _body = body env
          content_length = ("Content-Length", (show $ length _body))

closeConnection conn = hClose conn
