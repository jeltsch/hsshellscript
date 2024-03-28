module HsShellScript.ProcErr where

import Data.Maybe
import System.IO (Handle)
import Control.Concurrent.MVar
import GHC.IO.Handle.Internals             -- withHandle', do_operation


throwErrno' :: String -> Maybe Handle -> Maybe FilePath -> IO a

-- unsafeWithHandleFd  :: Handle -> (Fd -> IO a) -> IO a
-- unsafeWithHandleFd' :: Handle -> MVar Handle__ -> (Fd -> IO a) -> IO a
 

terminal_width_ioe :: Handle -> IO Int
terminal_width     :: Handle -> IO (Maybe Int)
