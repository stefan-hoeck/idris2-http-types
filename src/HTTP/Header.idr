module HTTP.Header

import Data.Buffer
import Data.ByteString
import Data.SortedMap as SM
import HTTP.MimeType
import Text.ILex.Util

%default total

export
record Headers where
  constructor MkHeaders
  headers : SortedMap ByteString ByteString

||| Converts a set of HTTP headers to a list of name-value pairs.
export %inline
kvList : Headers -> List (ByteString,ByteString)
kvList = kvList . headers

export %inline
emptyHeaders : Headers
emptyHeaders = MkHeaders empty

||| Inserts a name-value pair into a set of header.
|||
||| Since HTTP header names are case-insensitive, the name
||| will be converted to all upper-case letters.
export %inline
insertHeader : (name, value : ByteString) -> Headers -> Headers
insertHeader name value = {headers $= insert (toUpper name) value}

||| Looks up the header of the given name.
|||
||| `name` has to be in all upper-case letters.
export %inline
lookupUpperCaseHeader : (name : ByteString) -> Headers -> Maybe ByteString
lookupUpperCaseHeader n = lookup n . headers

||| Looks up the header of the given name.
|||
||| `name` is converted to all upper-case letters before
||| searching in the dictionary of headers. Use `lookupUpperCaseHeader`
||| if `name` is already in upper-case letters.
export %inline
lookupHeader : (name : ByteString) -> Headers -> Maybe ByteString
lookupHeader = lookupUpperCaseHeader . toUpper

--------------------------------------------------------------------------------
-- Specialized Utilities
--------------------------------------------------------------------------------

public export
Accept : ByteString
Accept = "ACCEPT"

public export
Content_Size : ByteString
Content_Size = "CONTENT-SIZE"

public export
Content_Type : ByteString
Content_Type = "CONTENT-TYPE"

||| Reads the `Accept` header and converts it to a list of media types.
export
accept : Headers -> List MimeType
accept =
    mapMaybe toMimeType
  . maybe [] (map trim . split 44)
  . lookupUpperCaseHeader Accept

||| Checks if the given media type can be handled according to
||| the given request headers.
export
acceptsMedia : Headers -> String -> Bool
acceptsMedia hs s =
 let ts := accept hs
  in any ((s ==) . type) ts || any (("*/*" ==) . type) ts

||| Reads the `Content-Type` header and converts it to a media type.
export %inline
contentType : Headers -> Maybe MimeType
contentType hs = lookupUpperCaseHeader Content_Type hs >>= toMimeType

||| Checks if the given media type corresponds to the media type
||| of the request's content.
export
hasContentType : Headers -> String -> Bool
hasContentType hs s = Just s == map type (contentType hs)

||| Reads the `Content-Size` header and converts it to a natural number
export
contentSize : Headers -> Nat
contentSize = maybe 0 (cast . decimal) . lookupUpperCaseHeader Content_Size
