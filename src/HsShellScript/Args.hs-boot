module HsShellScript.Args where

import Data.Typeable

data ArgError = ArgError {
      argerror_message :: String,
      argerror_usageinfo :: String
   }
   deriving (Typeable)
