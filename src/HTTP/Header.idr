module HTTP.Header

import Data.ByteString
import public Data.SortedMap as SM

%default total

||| Alias for a sorted map mapping header names to header values.
public export
0 Headers : Type
Headers = SortedMap ByteString ByteString
