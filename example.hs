import Shrimp
import Hack hiding (Response)
import Helpers

routes = [ ("/test", (\env -> 
                        case method env of
                            GET -> Response 200 "Got" defaultHeaders
                            POST -> Response 200 "Posted" defaultHeaders
                            _ -> Response 405 "Not Allowed" defaultHeaders
                     ))
         , ("/todo", (\env -> 
                    case method env of
                        GET ->
                            Response 200 "TODO" defaultHeaders)) ]

main = run "localhost" 8080 routes
