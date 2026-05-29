module HTTP.Debug

import HTTP.API.Decode
import HTTP.I18n.EN
import HTTP.URI
import Text.ILex

%default total

--------------------------------------------------------------------------------
-- Decode Testing
--------------------------------------------------------------------------------

||| Testing facility for value decoding.
|||
||| Example usage at the REPL:
|||
||| ```
||| :exec decodeTest Double "12.112"
||| ```
export
decodeTest : (0 a : Type) -> Decode a => Show a => String -> IO ()
decodeTest a =
 let loc := HTTPEN
  in either (putStrLn . interpolate) printLn . decodeAs a . fromString

||| Testing facility for path decoding.
|||
||| Example usage at the REPL:
|||
||| ```
||| :exec decodeTest (Vect 3 Nat) "https://www.hock.com/1/2/3?foo=bar"
||| ```
export
decodeManyTest : (0 a : Type) -> DecodeMany a => Show a => String -> IO ()
decodeManyTest a s =
 let loc := HTTPEN
  in case parseURI Virtual (fromString s) of
       Left err => putStrLn "\{err}"
       Right u  => case decodeMany {a} u.path of
         Right ([],v) => printLn v
         Right (b::bs,v) => putStrLn "only consumed up to \{b}: \{show v}"
         Left x => putStrLn "\{x}"
