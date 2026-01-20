module HTTP.Header

import Data.Buffer
import Data.ByteString
import Data.SortedMap as SM
import Data.String
import HTTP.Parser.Header
import Text.ILex
import Text.ILex.DStack

import public HTTP.Header.Types

%hide Data.Linear.(.)
%default total

export
record Headers where
  constructor MkHeaders
  headers : HeaderMap

||| Converts a set of HTTP headers to a list of name-value pairs.
export %inline
kvList : Headers -> List (String,ByteString)
kvList = kvList . headers

export %inline
emptyHeaders : Headers
emptyHeaders = MkHeaders empty

||| Inserts a name-value pair into a set of header.
|||
||| Since HTTP header names are case-insensitive, the name
||| will be converted to all upper-case letters.
export %inline
insertHeader : (name : String) -> ByteString -> Headers -> Headers
insertHeader name value = {headers $= insert (toUpper name) value}

||| Looks up the header of the given name.
|||
||| `name` has to be in all upper-case letters.
export %inline
lookupUpperCaseHeader : (name : String) -> Headers -> Maybe ByteString
lookupUpperCaseHeader n = lookup n . headers

||| Looks up the header of the given name.
|||
||| `name` is converted to all upper-case letters before
||| searching in the dictionary of headers. Use `lookupUpperCaseHeader`
||| if `name` is already in upper-case letters.
export %inline
lookupHeader : (name : String) -> Headers -> Maybe ByteString
lookupHeader = lookupUpperCaseHeader . toUpper

--------------------------------------------------------------------------------
-- Specialized Utilities
--------------------------------------------------------------------------------

public export
Accept : String
Accept = "ACCEPT"

public export
Authorization : String
Authorization = "AUTHORIZATION"

public export
Content_Length : String
Content_Length = "CONTENT-LENGTH"

public export
Content_Type : String
Content_Type = "CONTENT-TYPE"

public export
Content_Disposition : String
Content_Disposition = "CONTENT-DISPOSITION"

headerMay : {st : _} -> {x : _} -> String -> HRes st x t -> Headers -> Maybe t
headerMay nm res hs =
  lookupUpperCaseHeader nm hs >>= \bs => headerMay res bs

headerVal : {st : _} -> {x : _} -> String -> HRes st x t -> t -> Headers -> t
headerVal nm res v hs =
  fromMaybe v $ lookupUpperCaseHeader nm hs >>= \bs => headerMay res bs

||| Reads the `Accept` header and converts it to a list of media types.
export %inline
accept : Headers -> MediaRanges
accept = headerVal Accept RAcc []

||| Checks if the given media type can be handled according to
||| the given request headers.
export
acceptsMedia : Headers -> MediaType -> Bool
acceptsMedia hs mt = any (flip accepts mt . type) (accept hs)

||| Reads the `Content-Disposition` header and converts it to a media type.
export %inline
contentDisposition : Headers -> Maybe ContentDisp
contentDisposition = headerMay Content_Disposition RConD

||| Reads the `Content-Type` header and converts it to a media type.
export %inline
contentType : Headers -> Maybe ContentType
contentType = headerMay Content_Type RConT

||| Checks if the given media type corresponds to the media type
||| of the request's content.
export
hasContentType : Headers -> MediaType -> Bool
hasContentType hs t = Just t == map type (contentType hs)

||| Reads the `Content-Length` header and converts it to a natural number
export
contentLength : Headers -> Nat
contentLength = headerVal Content_Length RConL 0

export %inline
parseHeaders : Origin -> ByteString -> Either (ParseError Void) Headers
parseHeaders o = map MkHeaders . parseBytes (header RMap) o

export %inline
parseHeadersMay : ByteString -> Maybe Headers
parseHeadersMay = map MkHeaders . headerMay RMap

--------------------------------------------------------------------------------
-- Test Parsing
--------------------------------------------------------------------------------

export
testParseHeaders : ByteString -> IO ()
testParseHeaders =
  either (putStrLn . interpolate) printPairs . parseHeaders Virtual

  where
    printPairs : Headers -> IO ()
    printPairs hs = for_ (kvList hs) $ \(n,v) => putStrLn "\{n}: \{v}"
