module HTTP.API.Client.I18n.EN

import HTTP.API.Client
import HTTP.I18n.EN
import public HTTP.API.Client.I18n

%default total

serverErr : Bits16 -> String
serverErr s =
  """
  The server responded with status code \{show s}. This is a
  server-side error. Please try again in a moment. If this does not
  help, please get in touch with your server admin.
  """

parameters {auto lg : Logger JS}
  ||| Please note that this is an opinionated implementation of `JSLocal`.
  export
  [JSEN] JSLocal using HTTPEN where
    logJSErr x    =
      error
        """
        An error occurred in the user interface. This is probably a bug.

        Error details: \{dispErr x}
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
