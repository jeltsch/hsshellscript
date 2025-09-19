module HsShellScript.ProcErr where

import Data.Maybe
import System.IO (Handle)
import Control.Concurrent.MVar


throwErrno' :: String -> Maybe Handle -> Maybe FilePath -> IO a

terminal_width_ioe :: Handle -> IO Int
terminal_width     :: Handle -> IO (Maybe Int)
