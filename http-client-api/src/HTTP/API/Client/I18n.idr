module HTTP.API.Client.I18n

import HTTP.API.Client
import public HTTP.I18n
import public IO.Async.JS
import public IO.Async.Logging

%default total

public export
interface HTTPLocal => JSLocal where
  logJSErr   : JSErr -> Async JS es ()
  logHTTPErr : HTTPError -> Async JS es ()

export %inline
JSLocal => Loggable JS JSErr where logLoggable = logJSErr

export %inline
JSLocal => Loggable JS HTTPError where logLoggable = logHTTPErr
