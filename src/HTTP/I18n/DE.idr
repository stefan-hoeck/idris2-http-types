module HTTP.I18n.DE

import HTTP.API.Decode
import HTTP.RequestErr
import IO.Async.Logging
import public HTTP.I18n

%default total

export
[HTTPDE] HTTPLocal where
  endOfURIPath = "Unerwartetes Ende des URI-Pfads"
  floatingPointNumber = "Fliesskommazahl"
  integer = "Ganzzahl"
  invalidPath = "Ungültiger URI-Pfad"
  jsonValue = "JSON Wert"
  logLevel l =
    case l of
      Trace => "Ablaufverfolgung"
      Debug => "Fehlersuche"
      Info => "Information"
      Warn =>  "Warnung"
      Error => "Fehler"
      Fatal => "Schwerwiegender Fehler"
  missingBoundary = "Form-Data-Header ungültig: Grenzwert-Angabe fehlt"
  missingFormDataPart p ps = "Fehlender Form-Data-Teil: \{p} (Teil: \{ps})"
  missingHeader h = "Fehlender HTTP-Header"
  missingQueryParameter n = "Fehlender Abfrageparameter: '\{n}'"
  missingQueryValue n = "Fehlender Abfragewert: '\{n}'"
  myMediaTypeNotAccepted x y = "\{x} angegeben, aber es werden nur \{y} akzeptiert"
  unsignedInteger = "Ganzzahl ohne Vorzeichen"
  naturalNumber = "Natürliche Zahl"
  outOfBounds a b =
    "Wert außerhalb des gültigen Bereichs. Er muss zwischen \{show a} und \{show b} liegen."

  prettyRequestErr (RE s e m d p) =
    """
    Fehler Details:
    Status    : \{show s}
    Fehler    : \{e}
    Nachricht : \{m}
    Details   : \{d}
    Pfad      : \{p}
    """

  prettyDecodeErr (ReadErr t s d) = "Ungültig \{t}\{valueString s}"
  prettyDecodeErr (ContentErr t d) = "Ungültig \{t}"
  prettyDecodeErr (Msg msg) = msg
