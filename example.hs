import Shrimp

routes = [ ("/test", (\env -> Response 200 "Hello World" defaultHeaders))
         , ("/todo", (\env -> Response 200 "TODO" defaultHeaders)) ]

main = run 8080 routes
