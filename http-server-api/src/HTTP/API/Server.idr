module HTTP.API.Server

import public HTTP.API
import public HTTP.API.Server.Content
import public HTTP.API.Server.Interface
import public HTTP.API.Server.Method
import public HTTP.API.Server.Path
import public HTTP.API.Server.Query
import public HTTP.Prog
import public HTTP.Request
import public HTTP.Response

%default total

public export
data Server : APIs -> Type where
  Nil  : Server []
  (::) :
       {0 ts       : List Type}
    -> {0 as       : APIs}
    -> {endpoint   : HList ts}
    -> {auto all   : All Serve ts}
    -> {auto con   : HList (Constraints endpoint)}
    -> Endpoint endpoint
    -> Server as
    -> Server (endpoint :: as)

export
serveAll : (0 apis   : APIs) -> Server apis -> Request -> Handler Response
serveAll [] [] req = throw (requestErr notFound404)
serveAll (endpoint :: as) ((::) {endpoint} f x) req =
  case canServe endpoint req of
    True  => serveEndpoint endpoint f req
    False => serveAll as x req
