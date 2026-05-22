module HTTP.API.Path

%default total

||| Single part in a URI path.
|||
||| This matches either a specific string or one or more variables
||| stored at the given position in the path.
public export
data Part : Type where
  PScheme : String -> Part
  PAuth   : String -> Part
  PStr    : String -> Part
  PTill   : String -> Part
  Capture : (0 t : Type) -> Part

public export
FromString Part where fromString = PStr

||| Computes the list of types captured by a URI path description.
public export
0 PartsTypes : List Part -> List Type
PartsTypes []                = []
PartsTypes (PScheme _ :: xs) = PartsTypes xs
PartsTypes (PAuth _   :: xs) = PartsTypes xs
PartsTypes (PStr _    :: xs) = PartsTypes xs
PartsTypes (PTill _   :: xs) = PartsTypes xs
PartsTypes (Capture t :: xs) = t :: PartsTypes xs

||| Describes a pattern for matching and extracting
||| values for the path of the request URI.
public export
record ReqPath where
  constructor Path
  parts : List Part

public export
path : (scheme, authority : String) -> List Part -> ReqPath
path s a ps = Path $ PScheme s :: PAuth a :: ps
