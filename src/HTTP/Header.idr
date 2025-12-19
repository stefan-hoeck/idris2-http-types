module HTTP.Header

import Data.Buffer
import Data.ByteString
import HTTP.MimeType
import public Data.SortedMap as SM

%default total

||| Alias for a sorted map mapping header names to header values.
public export
0 Headers : Type
Headers = SortedMap ByteString ByteString

export
accept : Headers -> List MimeType
accept =
    mapMaybe toMimeType
  . maybe [] (map trim . split 44)
  . lookup "Accept"

export
acceptsMedia : Headers -> String -> Bool
acceptsMedia hs s =
 let ts := accept hs
  in any ((s ==) . type) ts || any (("*/*" ==) . type) ts

export %inline
contentType : Headers -> Maybe MimeType
contentType hs = lookup "Content-Type" hs >>= toMimeType

export
hasContentType : Headers -> String -> Bool
hasContentType hs s = Just s == map type (contentType hs)
