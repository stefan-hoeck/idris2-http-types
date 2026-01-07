module HTTP.URI

import Data.Buffer
import Derive.Prelude
import HTTP.Parser.URI
import Text.ILex

%hide Data.Linear.(.)
%default total
%language ElabReflection

public export
data QueryVal : Type where
  QVal   : (v : ByteString) -> QueryVal
  QEmpty : QueryVal

%runElab derive "QueryVal" [Show,Eq]

public export
0 Queries : Type
Queries = List (ByteString,QueryVal)

||| -- TODO: Special treatment of user info
public export
record URI where
  constructor MkURI
  scheme    : Maybe ByteString
  authority : Maybe ByteString
  abs       : Bool
  path      : List ByteString
  queries   : Queries
  fragment  : Maybe ByteString

%runElab derive "URI" [Show,Eq]

toQueries : ByteString -> Queries
toQueries = map qpair . split byte_ampersand
  where
    qpair : ByteString -> (ByteString, QueryVal)
    qpair bs =
      case break (byte_equals ==) bs of
        (pre, BS (S k) bv) => (pre, QVal $ BS k $ tail bv)
        (pre, _)           => (pre, QEmpty)

encodeQueries : Queries -> List ByteString
encodeQueries [] = []
encodeQueries ps = "?" :: intersperse "&" (map qpair ps)
  where
    qpair : (ByteString, QueryVal) -> ByteString
    qpair (n, v) =
     let en := uriEscape (not . isQueryNameByte) n
      in case v of
           QEmpty => en
           QVal x => fastConcat [en,"=",uriEscape (not . isQueryByte) x]

toURI : Part -> URI
toURI (P sch auth abs segs ques frag) =
  MkURI sch auth abs (segs <>> []) (maybe empty toQueries ques) frag

pathLines : URI -> List ByteString
pathLines (MkURI s a abs p q f) =
     (if abs || not (null a || null p) then ["/"] else [])
  ++ intersperse "/" (map (uriEscape (not . ispchar)) p)

export %inline
encodePath : URI -> ByteString
encodePath = fastConcat . pathLines

||| Properly escapes characters in a URL if necessary and
||| combines the different parts in the URI.
export
encodeURI : URI -> ByteString
encodeURI u@(MkURI s a _ _ q f) =
  fastConcat $
       maybe [] (::[":"]) s
    ++ maybe [] (\x => ["//", uriEscape (not . isAuthByte) x]) a
    ++ pathLines u
    ++ encodeQueries q
    ++ maybe [] (\x => ["#", uriEscape (not . isFragmentByte) x]) f

export %inline
parseURI : Origin -> ByteString -> Either (ParseError Void) URI
parseURI o = map toURI . parseBytes uri o

export %inline
Interpolation URI where
  interpolate = toString . encodeURI

--------------------------------------------------------------------------------
-- Test Parsing
--------------------------------------------------------------------------------

export
testParseURI : ByteString -> IO ()
testParseURI =
  either (putStrLn . interpolate) (putStrLn . interpolate) . parseURI Virtual
