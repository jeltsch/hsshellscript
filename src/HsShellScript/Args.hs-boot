module HsShellScript.Args where

import GHC.IO

data ArgError = ArgError {
      argerror_message :: String,
      argerror_usageinfo :: String
   }
   deriving (Typeable)


terminal_width_ioe :: Handle -> IO Int
terminal_width     :: Handle -> IO (Maybe Int)
