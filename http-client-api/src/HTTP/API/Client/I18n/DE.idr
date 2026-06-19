module HTTP.API.Client.I18n.DE

import HTTP.API.Client
import HTTP.I18n.DE
import public HTTP.API.Client.I18n

%default total

serverErr : Bits16 -> String
serverErr s =
  """
  Der Server hat mit dem Statuscode \{show s} geantwortet. Dies ist ein
  serverseitiger Fehler. Bitte versuchen Sie es in einigen Augenblicken
  erneut. Falls das Problem weiterhin besteht, wenden Sie sich bitte an
  Ihren Serveradministrator.
  """

parameters {auto lg : Logger JS}
  ||| Please note that this is an opinionated implementation of `JSLocal`.
  export
  [JSDE] JSLocal using HTTPDE where
    logJSErr x    =
      error
        """
        In der Benutzeroberfläche ist ein Fehler aufgetreten. Dabei handelt
        es sich vermutlich um einen Programmfehler.

        Fehlerdetails: \{dispErr x}
        """

    logHTTPErr Timeout         =
      error
        """
        Connection to the server timed out. That can happen when you are
        on a slow connection or the server is busy with other requests. If
        the situation does not improve, please get in touch with your
        server admin.
        """
    logHTTPErr NetworkError    =
      error
      """
      Error when connecting to the server. Please check your network
      connection and whether your VPN is correctly set up (if necessary). If this
      does not help, the server might be down. In that case, please
      get in touch with your server admin.
      """
    logHTTPErr (ReqError m)   =
      case cast {to = Bits16} m.status of
        403 => warn m.message
        401 => warn m.message
        409 => warn m.message
        s   => if s >= 500
          then error (serverErr s)
          else
            error
              """
              The server responded with status code \{show m.status}, which is
              unexpected and might be a bug. Please get in touch with your server admin
              and send them the following detailed error message:
              \{m}
              """
    logHTTPErr (DecError s x) =
      if s >= 500 then error (serverErr s)
      else case x of
        ContentErr t d => error
          """
          I got an error when decoding a response from the server. This is
          a bug. Please inform your server admin and send them the error
          message printed below:
          \{t}
          \{d}
          """
        x => error
          """
          I got an error when decoding a response from the server. This is
          a bug. Please inform your server admin and send them the error
          message printed below:
          \{x}
          """
