module HTTP.Method

import Derive.Prelude

%default total
%language ElabReflection

public export
data Method : Type where
  GET    : Method
  PUT    : Method
  POST   : Method
  DELETE : Method

%runElab derive "Method" [Show,Eq,Ord]
