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
        Die Verbindung zum Server ist abgelaufen. Dies kann passieren, wenn
        Ihre Internetverbindung langsam ist oder der Server mit anderen
        Anfragen ausgelastet ist. Sollte sich die Situation nicht verbessern,
        wenden Sie sich bitte an Ihren Serveradministrator.
        """

    logHTTPErr NetworkError    =
      error
        """
        Fehler bei der Verbindung zum Server. Bitte überprüfen Sie Ihre
        Netzwerkverbindung sowie gegebenenfalls die korrekte Einrichtung
        Ihres VPNs. Sollte dies nicht helfen, ist der Server möglicherweise
        nicht erreichbar. Wenden Sie sich in diesem Fall bitte an Ihren
        Serveradministrator.
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
              Der Server hat mit dem Statuscode \{show m.status} geantwortet.
              Dieser Statuscode ist unerwartet und könnte auf einen Programmfehler
              hinweisen. Bitte wenden Sie sich an Ihren Serveradministrator und
              übermitteln Sie die folgende detaillierte Fehlermeldung:
              \{m}
              """

    logHTTPErr (DecError s x) =
      if s >= 500 then error (serverErr s)
      else case x of
        ContentErr t d => error
          """
          Beim Verarbeiten der Serverantwort ist ein Fehler aufgetreten.
          Dies ist ein Programmfehler. Bitte informieren Sie Ihren
          Serveradministrator und übermitteln Sie die unten stehende
          Fehlermeldung:
          \{t}
          \{d}
          """
        x => error
          """
          Beim Verarbeiten der Serverantwort ist ein Fehler aufgetreten.
          Dies ist ein Programmfehler. Bitte informieren Sie Ihren
          Serveradministrator und übermitteln Sie die unten stehende
          Fehlermeldung:
          \{x}
          """
