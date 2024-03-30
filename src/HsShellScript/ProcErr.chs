-- #hide

-- New Typeable class in GHC 7.8:
-- http://www.haskell.org/ghc/docs/7.8.3/html/libraries/base-4.7.0.1/Data-Typeable.html
-- https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/PolyTypeable


module HsShellScript.ProcErr where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.IORef as IORef
import Data.Int
import Data.List
import Data.Maybe
import Data.Typeable
import Foreign
import Foreign.C
import Foreign.C.Error
import GHC.Conc
import GHC.IO hiding (finally, bracket)
import GHC.IO.Exception                    -- SystemError, ioe_*
import GHC.IO.Handle
import GHC.IO.Handle.Internals             -- withHandle', do_operation
import GHC.IO.Handle.Types hiding (close)
import Prelude hiding (catch)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Error hiding (catch)
import System.Posix
import System.Posix.IO
import System.Posix.Process (forkProcess)
import System.Posix.Types                  -- Fd
import qualified GHC.IO.FD as FD
import qualified System.IO.Error                -- mkIOError

import HsShellScript.Args
import HsShellScript.Shell

infixr 2 -|-    -- left handed, stdout
infixr 2 =|-    -- left handed, stderr
infixl 2 -|=    -- right handed, stdout
infixl 2 =|=    -- right handed, stderr
infixl 3 ->-    -- write stdout to file
infixl 3 =>-    -- write stderr to file
infixl 3 ->>-   -- append stdout to file
infixl 3 =>>-   -- append stderr to file
infixl 3 -<-    -- read stdin from file or string
infixl 3 -&>-   -- write stdout and stderr to file
infixl 3 -&>>-  -- append stdout and stderr to file





{- | Improved version of @System.Posix.Files.setFileMode@, which sets the file name in the @IOError@ which is
thrown in case of an error. The implementation in GHC 6.2.2 neglects to do this.

>setFileMode' path mode =
>   fill_in_filename path $
>      setFileMode path mode
-}
setFileMode' :: FilePath -> FileMode -> IO ()
setFileMode' path mode =
   fill_in_filename path $
      setFileMode path mode


-- |
-- Execute an IO action as a separate process, and wait for it to finish.
-- Report errors as exceptions.
--
-- This forks a child process, which performs the specified IO action. In case
-- the child process has been stopped by a signal, the parent blocks.
--
-- If the action throws an @IOError@, it is transmitted to the parent.
-- It is then raised there, as if it happened locally. The child then aborts
-- quietly with an exit code of 0.
--
-- Exceptions in the child process, other than @IOError@s, result in an error
-- message on @stderr@, and a @ProcessStatus@ exception in the parent, with the
-- value of @Exited (ExitFailure 1)@. The following exceptions are understood by
-- @subproc@, and result in corresponding messages: @ArgError@, @ProcessStatus@,
-- @RunError@, @IOError@ and @ExitCode@. Other exceptions result in the generic
-- message, as produced by @show@.
--
-- If the child process exits with an exit code other than zero, or it is
-- terminated by a signal, the corresponding @ProcessStatus@ is raised as an
-- exception in the parent program. Only @IOError@s are transmitted to the parent.
--
-- When used in conjunction with an @exec@ variant, this means that the parent
-- process can tell the difference between failure of the @exec@ call itself,
-- and failure of the child program being executed after a successful call of
-- the @exec@ variant. In case of failure of the @exec@
-- call, You get the @IOError@, which
-- happened in the child when calling @executeFile@ (from the GHC hierarchical
-- libraries). In case of the called program failing, you get the @ProcessStatus@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally (unless it throws an
-- @IOError@). The child process is then properly terminated by @subproc@, such
-- that no resources, which have been duplicated by the fork, cause problems.
-- See "HsShellScript#subr" for details.
--
-- If you want to run an external program, by calling one of the @exec@
-- variants in the child action, you might want to call @runprog@ instead of @subproc@.
--
--
-- Examples:
--
-- Run a program with the environment replaced:
--
-- >subproc (execpe "foobar" ["1","2","3"] new_env)
--
-- This results in a @ProcessStatus@ exception:
--
-- >subproc (exec "/bin/false" [])
--
-- This results in an @IOError@ (unless you actually have @\/frooble@):
--
-- >subproc (exec "/frooble" [])
--
-- See 'runprog', 'spawn', 'exec', 'execp', 'exece', 'execpe'.
subproc :: IO a                 -- ^ Action to execute in a child process
        -> IO ()
subproc io = do

  -- Make new error channel
   (readend, writeend) <- createPipe

   -- Set it to "close on exec"
   {#call c_close_on_exec#} (fromIntegral writeend)

   -- Fork child process
   flush_outerr
   pid <- forkProcess (do -- Child process
                          closeFd readend

                          -- Do it. In case some part of the child hands over an IOError to
                          -- be transmitted to the parent, do that and abort quietly.
                          child $
                             catch (io >> return ())
                                   (\(ioe::IOError) -> do
                                       send_ioerror writeend ioe
                                       flush_outerr
                                       _exit 0
                                   )
                      )

   -- Parent process
   closeFd writeend

   -- Read the complete contents of the error channel as an encoding
   -- of a possible IOError (until closed on the other side).
   --
   -- The write end in the child stays open, until either
   --    - exec in the child
   --    - child terminates (not merely stops)
   --    - child sends ioerror and closes the channel
   mioe <- receive_ioerror readend

   -- Waits for the child to finish. The process status is "Exited
   -- ExitSuccess" in case the child transmitted an error.
   (Just ps) <- getProcessStatus True False (fromIntegral pid)
   if ps == Exited ExitSuccess
       then return ()
       else throw ps

   -- In case an IOError has been received, throw it locally
   case mioe of
      Just ioe -> ioError ioe
      Nothing  -> return ()


-- |
-- Execute an IO action as a separate process, and wait for it to finish.
-- Report errors as exceptions.
--
-- /This function is included only for backwards compatibility. New code should/
-- /use/ 'subproc' instead/, which has better error handling./
--
-- The program forks a child process and performs the specified action.
-- Then it waits for the child process to finish. If it exits in any way
-- which indicates an error, the @ProcessStatus@ is thrown.
--
-- The parent process waits for the child processes, which have been stopped by
-- a signal.
--
-- See "HsShellScript#subr" for further details.
--
-- See 'subproc', 'spawn'.
call :: IO a  -- ^ action to execute as a child process
     -> IO ()
call io = do
    pid <- spawn_loc "call" io
    (Just ps) <- getProcessStatus True False pid
    if ps == Exited ExitSuccess
        then return ()
        else throw ps


-- |
-- Execute an IO action as a separate process, and continue without waiting
-- for it to finish.
--
-- The program forks a child process, which performs the specified action and terminates.
-- The child's process ID is returned.
--
-- See "HsShellScript#subr" for further details.
--
-- See 'subproc'.
spawn :: IO a           -- ^ Action to execute as a child process.
      -> IO ProcessID   -- ^ Process ID of the new process.
spawn = spawn_loc "spawn"

spawn_loc :: String -> IO a -> IO ProcessID
spawn_loc loc io = do
   flush_outerr
   pid <- forkProcess (child io)
   return pid


-- |
-- Run an external program. This starts a program as a child
-- process, and waits for it to finish. The executable is searched via the
-- @PATH@.
--
-- /This function is included for backwards compatibility only. New code should/
-- /use/ 'runprog'/, which has much better error handling./
--
-- When the specified program can't be executed, an error message is printed, and the main process
-- gets a @ProcessStatus@ thrown, with the value @Exited
-- (ExitFailure 1)@. This means that the main program can't distinguish between
-- failure of calling the program and the program exiting with an exit code of
-- 1. However, an error message \"Error calling ...\", including the description in the IOError produced
-- by the failed @execp@ call, is printed on @stderr@.
--
-- @run prog par@ is essentially @call (execp prog par)@.
--
-- Example:
--
-- >run "/usr/bin/foobar" ["some", "args"]
-- >   `catch` (\ps -> do -- oops...
-- >              )
--
-- See 'runprog', 'subproc', 'spawn'.
run :: FilePath                    -- ^ Name of the executable to run
    -> [String]                    -- ^ Command line arguments
    -> IO ()
run prog par =
   call (child $ execp prog par)



{- | An error which occured when calling an external program.
   The fields specifiy the details of the call.

   See 'show_runerror', 'to_ioe', 'as_ioe', @System.Posix.ProcessStatus@.
-}
data RunError = RunError
        { re_prog  :: String             -- ^ Program name
        , re_pars  :: [String]           -- ^ Program arguments
        , re_env   :: [(String,String)]  -- ^ The environment in use when the call was done
        , re_wd    :: String             -- ^ The working directory when the call was done
        , re_ps    :: ProcessStatus      -- ^ The process status of the failure
        , re_errno :: Maybe CInt         -- ^ The error (errno) code
        }
   deriving (Show, Typeable, Eq)

instance Exception RunError



-- | Make a readable error message. This includes all the
-- fields of @RunError@ except for the environment.
--
-- See 'RunError'.
show_runerror :: RunError -> String
show_runerror re =
   "The following program failed:\n\
   \   " ++ shell_command (re_prog re) (re_pars re) ++ "\n" ++
   explain_processstatus (re_ps re) ++ "\n\
   \The working directory was " ++ quote (re_wd re) ++ "."


-- | Generate a human-readable description of a @ProcessStatus@.
--
-- See 'exec', 'runprog' and @System.Posix.ProcessStatus@ in the GHC hierarchical
-- library documentation.
explain_processstatus :: ProcessStatus -> String
explain_processstatus ps =
   case ps of
      Exited (ExitFailure ec) -> "The program exited abnormally with an exit code of " ++ show ec ++ "."
      Exited ExitSuccess      -> "The program finished normally."
      Terminated sig _        -> "The process was terminated by signal " ++ showsig sig ++ "."
      Stopped sig             -> "The process was stopped by signal " ++ showsig sig ++ "."
   where
      showsig sig = show sig ++
                    case lookup sig signals of
                       Just name -> " (" ++ name ++ ")"
                       Nothing   -> ""

      signals = [(sigABRT, "SIGABRT"), (sigALRM, "SIGALRM"), (sigBUS, "SIGBUS"), (sigCHLD, "SIGCHLD"),
                 (sigCONT, "SIGCONT"), (sigFPE, "SIGFPE"), (sigHUP, "SIGHUP"), (sigILL, "SIGILL"),
                 (sigINT, "SIGINT"), (sigKILL, "SIGKILL"), (sigPIPE, "SIGPIPE"), (sigQUIT, "SIGQUIT"),
                 (sigSEGV, "SIGSEGV"), (sigSTOP, "SIGSTOP"), (sigTERM, "SIGTERM"), (sigTSTP, "SIGTSTP"),
                 (sigTTIN, "SIGTTIN"), (sigTTOU, "SIGTTOU"), (sigUSR1, "SIGUSR1"), (sigUSR2, "SIGUSR2"),
                 (sigPOLL, "SIGPOLL"), (sigPROF, "SIGPROF"), (sigSYS, "SIGSYS"), (sigTRAP, "SIGTRAP"),
                 (sigURG, "SIGURG"), (sigVTALRM, "SIGVTALRM"), (sigXCPU, "SIGXCPU"), (sigXFSZ, "SIGXFSZ")]


-- | Convert a @RunError@ to an @IOError@.
--
-- The @IOError@ type isn't capable of holding all the information which is
-- contained in a @RunError@. The environment is left out, and most of the other
-- fields are included only informally, in the description.
--
-- The fields of the generated @IOError@ are:
--
-- * The handle (@ioeGetHandle@): @Nothing@
--
-- * The error type (@ioeGetErrorType@): @GHC.IO.Exception.SystemError@
--
-- * @ioe_location@: @\"runprog\"@
--
-- * @ioe_description@: The error message, as procuded by @show_runerror@.
--
-- * @ioe_filename@: This is @Just (shell_command /prog/ /pars/)@, with /prog/
--   and /pars/ being the program and its arguments.
--
-- See 'as_ioe', 'runprog', 'show_runerror'.
to_ioe :: RunError -> IOError
to_ioe re =
   GHC.IO.Exception.IOError { ioe_handle      = Nothing,
                              ioe_type        = GHC.IO.Exception.SystemError,
                              ioe_location    = "runprog",
                              ioe_description = show_runerror re,
                              ioe_filename    = Just (shell_command (re_prog re) (re_pars re)),
                              ioe_errno       = re_errno re
                            }


-- | Call the specified IO action (which is expected to contain calls of
-- @runprog@) and convert any @RunError@ exceptions to @IOError@s.
--
-- The conversion is done by @to_ioe@.
--
-- See 'to_ioe', 'runprog'.
as_ioe :: IO a -> IO a
as_ioe io =
   io
   `catch` (\(re::RunError) -> ioError (to_ioe re))


-- |
-- Run an external program, and report errors as exceptions. The executable is
-- searched via the @PATH@. In case the child process has been stopped by a
-- signal, the parent blocks.
--
-- In case the program exits in an way which indicates an error, or is
-- terminated by a signal, a @RunError@ is thrown. It
-- contains the details of the call. The @runprog@ action can also be converted
-- to throw @IOError@s instaed, by applying @as_ioe@ to it. Either can be used
-- to generate an informative error message.
--
-- In case of starting the program itself failed, an @IOError@ is thrown.
--
-- @runprog prog par@ is a simple front end to @subproc@. It is essentially
-- @subproc (execp prog par)@, apart from building a @RunError@ from a
-- @ProcessStatus@.
--
-- Example 1:
--
-- >do runprog "foo" ["some", "args"]
-- >   ...
-- >`catch` (\re -> do errm (show_runerror re)
-- >                      ...
-- >           )
--
-- Example 2:
--
-- >do as_ioe $ runprog "foo" ["some", "args"]
-- >   ...
-- >`catch` (\ioe -> do errm (show_ioerror ioe)
-- >                       ...
-- >           )
--
-- See 'subproc', 'spawn', 'RunError', 'show_runerror', 'to_ioe', 'as_ioe'.
runprog :: FilePath                    -- ^ Name of the executable to run
        -> [String]                    -- ^ Command line arguments
        -> IO ()
runprog prog pars =
   subproc (execp prog pars)

   `catch`
      -- Convert ProcessStatus error to RunError
      (\(ps::ProcessStatus) ->
          do env   <- System.Environment.getEnvironment
             wd    <- getCurrentDirectory
             (Errno c_errno) <- getErrno
             throw (RunError { re_prog  = prog
                             , re_pars  = pars
                             , re_env   = env
                             , re_wd    = wd
                             , re_ps    = ps
                             , re_errno = if c_errno /= (0::CInt) then Just c_errno else Nothing
                             }))



-- | Print an action as a shell command, then perform it.
--
-- This is used with actions such as 'runprog', 'exec' or 'subproc'. For instance,
-- @echo runprog prog args@ is a variant of @runprog prog args@, which prints what
-- is being done before doing it.
--
-- See 'runprog', 'subproc', 'exec'.
echo :: ( FilePath -> [String] -> IO () )       -- ^ Action to perform
     -> FilePath                                -- ^ Name or path of the executable to run
     -> [String]                                -- ^ Command line arguments
     -> IO ()
echo action path args = do
   putStrLn (shell_command path args)
   action path args


-- | Execute an external program. This replaces the running process. The path isn't searched, the environment
-- isn't changed. In case of failure, an IOError is thrown.
--
-- >exec path args =
-- >   execute_file path False args Nothing
--
-- See 'execute_file', "HsShellScript#exec".
exec :: String          -- ^ Full path to the executable
     -> [String]        -- ^ Command line arguments
     -> IO a            -- ^ Never returns
exec path args =
   execute_file path False args Nothing


-- | Execute an external program. This replaces the running process. The path is searched, the environment
-- isn't changed. In case of failure, an IOError is thrown.
--
-- >execp prog args =
-- >   execute_file prog True args Nothing
--
-- See 'execute_file', "HsShellScript#exec".
execp :: String        -- ^ Name or path of the executable
      -> [String]      -- ^ Command line arguments
      -> IO a          -- ^ Never returns
execp prog args =
   execute_file prog True args Nothing


-- | Execute an external program. This replaces the running process. The path isn't searched, the environment
-- of the program is set as specified. In case of failure, an IOError is thrown.
--
-- >exece path args env =
-- >   execute_file path False args (Just env)
--
-- See 'execute_file', "HsShellScript#exec".
exece :: String                 -- ^ Full path to the executable
      -> [String]               -- ^ Command line arguments
      -> [(String,String)]      -- ^ New environment
      -> IO a                   -- ^ Never returns
exece path args env =
   execute_file path False args (Just env)


-- | Execute an external program. This replaces the running process. The path is searched, the environment of
-- the program is set as specified. In case of failure, an IOError is thrown.
--
-- >execpe prog args env =
-- >   execute_file prog True args (Just env)
--
-- See 'execute_file', "HsShellScript#exec".
execpe :: String                -- ^ Name or path of the executable
       -> [String]              -- ^ Command line arguments
       -> [(String,String)]     -- ^ New environment
       -> IO a                  -- ^ Never returns
execpe prog args env =
   execute_file prog True args (Just env)


{- | Build left handed pipe of stdout.

   \"@p -|- q@\" builds an IO action from the two IO actions @p@ and @q@.
   @q@ is executed in an external process. The standard output of @p@ is sent
   to the standard input of @q@ through a pipe. The result action consists
   of forking off @q@ (connected with a pipe), and @p@.

   The result action does /not/ run @p@ in a separate process. So, the pipe
   itself can be seen as a modified action @p@, forking a connected @q@. The
   pipe is called \"left handed\", because @p@ remains unforked, and not @q@.

   /The exit code of q is silently ignored./ The process ID of the forked
   copy of @q@ isn't returned to the caller, so it's lost.

   The pipe, which connects @p@ and @q@, is in /text mode/. This means that the 
   output of @p@ is converted from Unicode to the system character set, which 
   is determined by the environment variable @LANG@.

   See "HsShellScript#subr" and
   "HsShellScript#exec" for further details.

   Examples:

   >subproc (exec "/usr/bin/foo" [] -|- exec "/usr/bin/bar" [])

   >sunproc (    execp "foo" ["..."]
   >         -|= ( -- Do something with foo's output
   >               do cnt <- lazy_contents "-"
   >                  ...
   >             )
   >        )

   >sunproc ( err_to_out foo
   >          -|- exec "/usr/bin/tee" ["-a", "/tmp/foo.log"] )

   See 'subproc', '(=|-)', '(-|=)', 'redirect'
-}
(-|-) :: IO a   -- ^ Action which won't be forked
      -> IO b   -- ^ Action which will be forked and connected with a pipe
      -> IO a   -- ^ Result action
p -|- q = do
   (Just h, _, _, _) <- pipe_fork_dup q True False False
   res <- redirect stdout h p
   hClose h
   return res


{- | Build left handed pipe of stderr.

   \"@p =|- q@\" builds an IO action from the two IO actions @p@ and @q@.
   @q@ is executed in an external process. The standard error output of @p@ is sent
   to the standard input of @q@ through a pipe. The result action consists
   of forking off @q@ (connected with a pipe), and @p@.

   The result action does /not/ run @p@ in a separate process. So, the pipe
   itself can be seen as a modified action @p@, forking a connected @q@. The
   pipe is called \"left handed\", because @p@ has this property, and not @q@.

   /The exit code of q is silently ignored./ The process ID of the forked
   copy of @q@ isn't returned to the caller, so it's lost.

   The pipe, which connects @p@ and @q@, is in /text mode/. This means that the 
   output of @p@ is converted from Unicode to the system character set, which 
   is determined by the environment variable @LANG@.

   See "HsShellScript#subr" and
   "HsShellScript#exec" for further details.

   Example:

>subproc (exec "/usr/bin/foo" [] =|- exec "/usr/bin/bar" [])

   See 'subproc', '(-|-)', '(-|=)'.
-}
(=|-) :: IO a    -- ^ Action which won't be forked
      -> IO b    -- ^ Action which will be forked and connected with a pipe
      -> IO a    -- ^ Result action
p =|- q = do
   (Just h, _, _, _) <- pipe_fork_dup q True False False
   res <- redirect stderr h p
   hClose h
   return res


{- | Build right handed pipe of stdout.

   \"@p -|= q@\" builds an IO action from the two IO actions @p@ and @q@.
   @p@ is executed in an external process. The standard output of @p@ is sent
   to the standard input of @q@ through a pipe. The result action consists
   of forking off @p@ (connected with a pipe), and @q@.

   The result action does /not/ run @q@ in a separate process. So, the pipe
   itself can be seen as a modified action @q@, forking a connected @p@.
   The pipe is called \"right
   handed\", because @q@ has this property, and not @p@.

   /The exit code of p is silently ignored./ The process ID of the forked
   copy of @q@ isn't returned to the caller, so it's lost.

   The pipe, which connects @p@ and @q@, is in /text mode/. This means that the 
   output of @p@ is converted from Unicode to the system character set, which 
   is determined by the environment variable @LANG@.

   See "HsShellScript#subr" and
   "HsShellScript#exec" for further details.

   Example:

>subproc (exec \"\/usr\/bin\/foo\" [] -|= exec \"\/usr\/bin\/bar\" [])

   See 'subproc', '(=|-)', '(=|=)'.
-}
(-|=) :: IO a     -- ^ Action which will be forked and connected with a pipe
      -> IO b     -- ^ Action which won't be forked
      -> IO b     -- ^ Result action
p -|= q = do
   (_, Just h, _, _) <- pipe_fork_dup p False True False
   res <- redirect stdin h q
   hClose h
   return res

{- | Build right handed pipe of stderr.

   \"@p =|= q@\" builds an IO action from the two IO actions @p@ and @q@.
   @p@ is executed in an external process. The standard error output of @p@ is sent
   to the standard input of @q@ through a pipe. The result action consists
   of forking off @p@ (connected with a pipe), and @q@.

   The result action does /not/ run @q@ in a separate process. So, the pipe
   itself can be seen as a modified action @q@, forking a connected @p@.
   The pipe is called \"right
   handed\", because @q@ has this property, and not @p@.

   /The exit code of p is silently ignored./ The process ID of the forked
   copy of @q@ isn't returned to the caller, so it's lost.

   The pipe, which connects @p@ and @q@, is in /text mode/. This means that the 
   output of @p@ is converted from Unicode to the system character set, which 
   is determined by the environment variable @LANG@.

   See "HsShellScript#subr" and
   "HsShellScript#exec" for further details.

   Example:

   > subproc (exec "/usr/bin/foo" [] =|= exec "/usr/bin/bar" [])

   See 'subproc', '=|-', '-|='.
-}
(=|=) :: IO a     -- ^ Action which will be forked and connected with a pipe
      -> IO b     -- ^ Action which won't be forked
      -> IO b     -- ^ Result action
p =|= q = do
   (_, _, Just h, _) <- pipe_fork_dup p False False True
   res <- redirect stdin h q
   hClose h
   return res


-- | Temporarily replace a handle. This makes a backup copy of the original handle (typically a standard
-- handle), overwrites it with the specified one, runs the specified action, and restores the handle from the
-- backup.
--
-- Example:
--
-- >   h <- openFile "/tmp/log" WriteMode
-- >   redirect stdout h io
-- >   hClose h
--
-- This is the same as
--
-- >   io ->- "/tmp/log"
--
-- See '-|-', '=|-'.
redirect :: Handle              -- ^ Handle to replace
         -> Handle              -- ^ Handle to replace it with
         -> IO a                -- ^ Action
         -> IO a
redirect handle replacement io =
   bracket (do bak <- hDuplicate handle
               hDuplicateTo replacement handle
               return bak
           )
           (\bak -> do hDuplicateTo bak handle
                       hClose bak
           )
           (\_ -> io)


redirect_helper stdh mode io path = do
   h <- openFile path mode

   -- The file in a redirection is accessed in /text mode/, If stdout or stderr
   -- are redirected, this means that output is converted from ghc's Unicode to
   -- the system character set. If stdin is redirected, this means that data
   -- read from the file is converted from the system character set to ghc's
   -- Unicode. The system character set is taken from the environment variable
   -- LANG.
   hSetBinaryMode h False

   res <- redirect stdh h io
   hClose h
   return res


{- | Redirect the standard output of the specified IO action to a file. The file will be overwritten, if it
already exists.

What's actually modified is the @stdout@ handle, not the file descriptor 1. The
@exec@ functions know about this. See "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

The file is written in /text mode/. This means that the
output is converted from Unicode to the system character set, which
is determined by the environment variable @LANG@.

Example:

>runprog "/some/program" [] ->- "/tmp/output"

Note: You can't redirect to @\"\/dev\/null\"@ this way, because GHC 6.4's @openFile@ throws an \"invalid argument\"
IOError. (This may be a bug in the GHC 6.4 libraries). Use @->>-@ instead.

See 'subproc', 'runprog', '->>-', '=>-'.
-}
(->-) :: IO a           -- ^ Action, whose output will be redirected
      -> FilePath       -- ^ File to redirect the output to
      -> IO a           -- ^ Result action
(->-) io path =
   redirect_helper stdout WriteMode io path


{- | Redirect the standard output of the specified IO action to a file. If the file already exists, the output
will be appended.

What's actually modified is the @stdout@ handle, not the file descriptor 1. The
@exec@ functions know about this. See "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

The file is written in /text mode/. This means that the
output is converted from Unicode to the system character set, which
is determined by the environment variable @LANG@.

Example:

>run "/some/noisy/program" [] ->>- "/dev/null"

See 'subproc', 'runprog', '(->-)', '(=>>-)'.
-}
(->>-) :: IO a          -- ^ Action, whose output will be redirected
       -> FilePath      -- ^ File to redirect the output to
       -> IO a          -- ^ Result action
(->>-) io path =
   redirect_helper stdout AppendMode io path


{- | Redirect the standard error output of the specified IO action to a file. If the file already exists, it
will be overwritten.

What's actually modified is the @stderr@ handle, not the file descriptor 2. The
@exec@ functions know about this. See "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

Note: You can't redirect to @\"\/dev\/null\"@ this way, because GHC 6.4's @openFile@ throws an \"invalid argument\"
IOError. (This may be a bug in the GHC 6.4 libraries). Use @=>>-@ instead.

The file is written in /text mode/. This means that the
output is converted from Unicode to the system character set, which
is determined by the environment variable @LANG@.

Example:

>run "/path/to/foo" [] =>- "/tmp/errlog"

See 'subproc', 'runprog', '(->-)', '(=>>-)'.
-}
(=>-) :: IO a           -- ^ Action, whose error output will be redirected
      -> FilePath       -- ^ File to redirect the error output to
      -> IO a           -- ^ Result action
(=>-) =
   redirect_helper stderr WriteMode


{- | Redirect the standard error output of the specified IO action to a file. If the file already exists, the
output will be appended.

What's actually modified is the @stderr@ handle, not the file descriptor 2. The
@exec@ functions know about this. See "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

The file is written in /text mode/. This means that the
output is converted from Unicode to the system character set, which
is determined by the environment variable @LANG@.

Example:

>run "/some/program" [] =>>- "/dev/null"

See 'subproc', 'runprog', '(->>-)', '(=>-)'.
-}
(=>>-) :: IO a          -- ^ Action, whose error output will be redirected
       -> FilePath      -- ^ File to redirect the error output to
       -> IO a          -- ^ Result action
(=>>-) =
   redirect_helper stderr AppendMode


{- | Redirect both stdout and stderr to a file. This is equivalent to the
shell's @&>@ operator. If the file already exists, it will be overwritten.

What's actually modified are the @stdout@ and @stderr@ handles, not the file
descriptors 1 and 2. The @exec@ functions know about this. See
"HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

Note: You can't redirect to @\"\/dev\/null\"@ this way, because GHC 6.4's @openFile@ throws an \"invalid argument\"
IOError. (This may be a bug in the GHC 6.4 libraries). Use @-&>>-@ instead.

The file is written in /text mode/. This means that the
output is converted from Unicode to the system character set, which
is determined by the environment variable @LANG@.

>(-&>-) io path = err_to_out io ->- path

Example:

@subproc (exec \"\/path\/to\/foo\" [] -&\>- \"log\")@

See '(-&>>-)', 'err_to_out'.
-}
(-&>-) :: IO a          -- ^ Action, whose output and error output will be redirected
       -> FilePath      -- ^ File to redirect to
       -> IO a          -- ^ Result action
(-&>-) io path = err_to_out io ->- path


{- | Redirect both stdout and stderr to a file. If the file already exists, the
   output will be appended.

What's actually modified are the @stdout@ and @stderr@ handles, not the file
descriptors 1 and 2. The @exec@ functions know about this. See
"HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

The file is written in /text mode/. This means that the
output is converted from Unicode to the system character set, which
is determined by the environment variable @LANG@.

>(-&>>-) io path = (err_to_out >> io) ->>- path

Example:

>run "/some/noisy/program" [] -&>>- "/dev/null"

See '(-&>-)', 'out_to_err'.
-}
(-&>>-) :: IO a         -- ^ Action, whose output and error output will be redirected
       -> FilePath      -- ^ File to redirect to
       -> IO a          -- ^ Result action
(-&>>-) io path =
   err_to_out io ->>- path


{- | Redirect stdin from a file. This modifies the specified action, such
that the standard input is read from a file.

   What's actually modified is the @stdin@ handle, not the file
   descriptor 0. The @exec@ functions know about this. See
   "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

The file is read in /text mode/. This means that the input is converted from the
system character set to Unicode. The system's character set is determined by the
environment variable @LANG@.

Example:

@subproc (exec \"\/path\/to\/foo\" [] -\<- \"bar\")@

See 'exec', 'runprog', '(->-)', '(=>-)'.
-}
(-<-) :: IO a
      -> FilePath
      -> IO a
(-<-) = redirect_helper stdin ReadMode


{- | Send the error output of the specified action to its standard output.

What's actually modified is the @stdout@ handle, not the file descriptor 1. The
@exec@ functions know about this. See "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

>err_to_out = redirect stderr stdout

See 'redirect'.
-}
err_to_out :: IO a -> IO a
err_to_out = redirect stderr stdout


{- | Send the output of the specified action to its standard error output.

What's actually modified is the @stderr@ handle, not the file descriptor 2. The
@exec@ functions know about this. See "HsShellScript#fdpipes" and
"HsShellScript#exec" for details.

>redirect stdout stderr

See 'redirect'.
-}
out_to_err :: IO a -> IO a
out_to_err = redirect stdout stderr


-- Run an IO action as a new process, and optionally connect its
-- stdin, stdout and stderr via pipes.
pipe_fork_dup :: IO a                   -- Action to run in a new process.
              -> Bool                   -- make stdin pipe?
              -> Bool                   -- make stdout pipe?
              -> Bool                   -- make stderr pipe?
              -> IO ( Maybe Handle      -- Handle to the new process's stdin, if applicable.
                    , Maybe Handle      -- Handle to the new process's stdout, if applicable.
                    , Maybe Handle      -- Handle to the new process's stderr, if applicable.
                    , ProcessID
                    )
pipe_fork_dup io fd0 fd1 fd2 = do
    flush_outerr

    pipe0 <- pipe_if fd0
    pipe1 <- pipe_if fd1
    pipe2 <- pipe_if fd2

    pid <- forkProcess (do -- child
                           dup_close pipe0 stdin True
                           dup_close pipe1 stdout False
                           dup_close pipe2 stderr False
                           child io
                       )
    -- parent
    h0 <- finish_pipe pipe0 True
    h1 <- finish_pipe pipe1 False
    h2 <- finish_pipe pipe2 False
    return (h0, h1, h2, pid)

  where
     -- Make a pipe, if applicable.
     pipe_if False = return Nothing
     pipe_if True  = do
        (read, write) <- createPipe
        return (Just (read,write))

     -- Child work after fork: connect a fd of the new process to the pipe.
     dup_close :: Maybe (Fd, Fd)        -- maybe the pipe
               -> Handle                -- which handle to connect to the pipe
               -> Bool                  -- whether the child reads from this pipe
               -> IO ()
     dup_close Nothing _ _ =
         return ()
     dup_close m@(Just (readend,writeend)) dest True =
         do
            h <- System.Posix.fdToHandle readend
            hDuplicateTo h dest
            hSetBinaryMode dest False -- Use Text mode for the new handle by default. 
            hClose h
            closeFd writeend
     dup_close m@(Just (readend,writeend)) dest False =
         do 
            h <- System.Posix.fdToHandle writeend
            hDuplicateTo h dest
            hSetBinaryMode dest False -- Use Text mode for the new handle by default. 
            hClose h
            closeFd readend

     -- Parent work after fork: close surplus end of the pipe and make a handle from the other end.
     finish_pipe :: Maybe (Fd, Fd)      -- maybe the pipe
                 -> Bool                -- whether the fd is for reading
                 -> IO (Maybe Handle)
     finish_pipe Nothing _ =
         return Nothing
     finish_pipe (Just (readend,writeend)) read =
         do closeFd (if read then readend else writeend)
            let fd = if read then writeend else readend
            h <- System.Posix.fdToHandle fd
            -- Use Text mode for the new handle by default.
            hSetBinaryMode h False
            return (Just h)


-- |
-- Run an IO action as a separate process, and pipe some text to its @stdin@.
-- Then close the pipe and wait for the child process to finish.
--
-- This forks a child process, which executes the specified action. The specified
-- text is sent to the action's @stdin@ through a pipe. Then the pipe is closed.
-- In case the action replaces the process by calling an @exec@ variant, it is
-- made sure that the process gets the text on it's file descriptor 0.
--
-- In case the action fails (exits with an exit status other than 0, or is
-- terminated by a signal), the @ProcessStatus@ is thrown, such as reported by
-- 'System.Posix.getProcessStatus'. No attempt is made to create more meaningful
-- exceptions, like it is done by @runprog@/@subproc@.
--
-- Exceptions in the action result in an error message on @stderr@, and the
-- termination of the child. The parent gets a @ProcessStatus@ exception, with
-- the value of @Exited (ExitFailure 1)@. The following exceptions are
-- understood, and result in corresponding messages: @ArgError@,
-- @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other exceptions
-- result in the generic message, as produced by @show@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally.
-- The child process is then properly terminated, such
-- that no resources, which have been duplicated by the fork, cause problems.
-- See "HsShellScript#subr" for details.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in
-- the text are converted to the system character set. If you need to pipe binary
-- data, you should use @h_pipe_to@, and set the returned handle to binary
-- mode. This is accomplished by @'hSetBinaryMode' h True@. The system
-- character set is determined by the environment variable @LANG@.
--
-- Example:
--
-- >pipe_to "blah" (exec "/usr/bin/foo" ["bar"])
--
-- Example: Access both @stdin@ and @stdout@ of an external program.
--
-- >import HsShellScript
-- >
-- >main = mainwrapper $ do
-- >
-- >   res <- pipe_from $
-- >      pipe_to "2\n3\n1" $
-- >         exec "/usr/bin/sort" []
-- >
-- >   putStrLn res
--
--
-- See 'subproc', 'runprog', '-<-', 'h_pipe_to'.
pipe_to :: String       -- ^ Text to pipe
        -> IO a         -- ^ Action to run as a separate process, and to pipe to
        -> IO ()
pipe_to str io = do
   (h, pid) <- h_pipe_to io
   hPutStr h str
   hClose h
   (Just ps) <- getProcessStatus True False pid
   if ps == Exited ExitSuccess
       then return ()
       else throw ps


-- |
-- Run an IO action as a separate process, and get a connection (a pipe) to
-- its @stdin@ as a file handle.
--
-- This forks a subprocess, which executes the specified action. A file handle,
-- which is connected to its @stdin@, is returned. The child's @ProcessID@
-- is returned as well. If the action replaces the child process, by calling an
-- @exec@ variant, it is made sure that its file descriptor 0 is connected to
-- the returned file handle.
--
-- This gives you full control of the pipe, and of the forked process. But you
-- need to deal with the child process by yourself.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally.
-- The child process is then properly terminated, such
-- that no resources, which have been duplicated by the fork, cause problems.
-- See "HsShellScript#subr" for details.
--
-- Errors can only be detected by examining the child's process status (using
-- 'System.Posix.Process.getProcessStatus'). If the child action throws an
-- exception, an error message is printed on @stderr@, and the child process
-- exits with a @ProcessStatus@ of @Exited
-- (ExitFailure 1)@. The following exceptions are understood, and
-- result in corresponding messages: @ArgError@, @ProcessStatus@, @RunError@,
-- @IOError@ and @ExitCode@. Other exceptions result in the generic message, as
-- produced by @show@.
--
-- If the child process exits in a way which signals an error, the
-- corresponding @ProcessStatus@ is returned by @getProcessStatus@. See
-- 'System.Posix.Process.getProcessStatus' for details.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text are converted to the system character set. You can set the returned
-- handle to binary mode, by calling @'hSetBinaryMode' handle True@. The system
-- character set is determined by the environment variable @LANG@.
--
-- Example:
--
-- >(handle, pid) <- h_pipe_to $ exec "/usr/bin/foo" ["bar"]
-- >hPutStrLn handle "Some text to go through the pipe"
-- >(Just ps) <- getProcessStatus True False pid
-- >when (ps /= Exited ExitSuccess) $
-- >   throw ps
--
-- See '-<-', 'pipe_to', 'pipe_from', 'pipe_from2'. See "HsShellScript#fdpipes" for more details.
h_pipe_to :: IO a                       -- ^ Action to run as a separate process, and to pipe to
          -> IO (Handle, ProcessID)     -- ^ Returns handle connected to the standard input of the child process,
                                        --   and the child's process ID
h_pipe_to io = do
   (Just h, _, _, pid) <- pipe_fork_dup io True False False
   return (h, pid)


-- | Run an IO action as a separate process, and read its @stdout@ strictly.
-- Then wait for the child process to finish. This is like the backquote feature
-- of shells.
--
-- This forks a child process, which executes the specified action. The output
-- of the child is read from its standard output. In case it replaces the
-- process by calling an @exec@ variant, it is make sure that the output is
-- read from the new process' file descriptor 1.
--
-- The end of the child's output is reached when either the standard output is
-- closed, or the child process exits. The program blocks until the action
-- exits, even if the child closes its standard output earlier. So the parent
-- process always notices a failure of the action (when it exits in a way which
-- indicates an error).
--
-- When the child action exits in a way which indicates an error, the
-- corresponding @ProcessStatus@ is thrown. See
-- 'System.Posix.Process.getProcessStatus'. No attempt is made to create more
-- meaningful exceptions, like it is done by @runprog@/@subproc@.
--
-- Exceptions in the action result in an error message on @stderr@, and the
-- proper termination of the child. The parent gets a @ProcessStatus@ exception, with
-- the value of @Exited (ExitFailure 1)@. The following exceptions are
-- understood, and result in corresponding messages: @ArgError@,
-- @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other exceptions
-- result in the generic message, as produced by @show@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally. The child process is
-- then properly terminated, such that no resources, which have been duplicated
-- by the fork, cause problems. See "HsShellScript#subr" for details.
--
-- Unlike shells\' backquote feature, @pipe_from@ does not remove any trailing
-- newline characters. The entire output of the action is returned. You might want
-- to apply @chomp@ to the result.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from stdin, is converted from the system character set to
-- Unicode. The system character set is determined by the environment variable
-- @LANG@. If you need to read binary data from the forked process, you should use
-- @h_pipe_from@ and set the returned handle to binary mode. This is
-- accomplished by @'hSetBinaryMode' h True@.
--
-- Example:
--
-- >output <- pipe_from $ exec "/bin/mount" []
--
-- Example: Access both @stdin@ and @stdout@ of an external program.
--
-- >import HsShellScript
-- >
-- >main = mainwrapper $ do
-- >
-- >   res <- pipe_from $
-- >      pipe_to "2\n3\n1" $
-- >         exec "/usr/bin/sort" []
-- >
-- >   putStrLn res
--
-- See 'exec', 'pipe_to', 'pipe_from2', 'h_pipe_from', 'lazy_pipe_from', 'HsShellScript.Misc.chomp', 'silently'.
pipe_from :: IO a               -- ^ Action to run as a separate process. Its
                                -- return value is ignored.
          -> IO String          -- ^ The action's standard output
pipe_from io = do
   (h, pid) <- h_pipe_from io
   txt <- hGetContents h
   seq (length txt) (hClose h)
   (Just ps) <- System.Posix.getProcessStatus True False pid
   if ps == Exited ExitSuccess
       then return txt
       else throw ps


-- | Run an IO action as a separate process, and read its standard error output
-- strictly. Then wait for the child process to finish. This is like the
-- backquote feature of shells. This function is exactly the same as
-- @pipe_from@, except that the standard error output is read, instead of the
-- standard output.
--
-- This forks a child process, which executes the specified action. The error output
-- of the child is read from its standard error output. In case it replaces the
-- process by calling an @exec@ variant, it is made sure that the output is
-- read from the new process' file descriptor 2.
--
-- The end of the child's error output is reached when either the standard error
-- output is closed, or the child process exits. The program blocks until the
-- action exits, even if the child closes its standard error output earlier. So
-- the parent process always notices a failure of the action (which means it
-- exits in a way which indicates an error).
--
-- When the child action exits in a way which indicates an error, the
-- corresponding @ProcessStatus@ is thrown. See
-- 'System.Posix.Process.getProcessStatus'.
-- No attempt is made to create
-- more meaningful exceptions, like it is done by @runprog@/@subproc@.
--
--
-- Exceptions in the action result in an error message on @stderr@, and the
-- proper termination of the child. This means that the error message is sent
-- through the pipe, to the parent process. The message can be found in the text
-- which has been read from the child process. It doesn't appear on the console.
--
-- The parent gets a @ProcessStatus@ exception, with
-- the value of @Exited (ExitFailure 1)@. The following exceptions are
-- understood, and result in corresponding messages: @ArgError@,
-- @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other exceptions
-- result in the generic message, as produced by @show@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally. The child process is
-- then properly terminated, such that no resources, which have been duplicated
-- by the fork, cause problems. See "HsShellScript#subr" for details.
--
-- Unlike shells\' backquote feature, @pipe_from2@ does not remove any trailing
-- newline characters. The entire error output of the action is returned. You might want
-- to apply @chomp@ to the result.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from stdin, is converted from the system character set to
-- Unicode. The system character set is determined by the environment variable
-- @LANG@. If you need to read binary data from the forked process, you should use
-- @h_pipe_from@ and set the returned handle to binary mode. This is
-- accomplished by @'hSetBinaryMode' h True@.
--
-- Example:
--
-- >output <- pipe_from $ exec "/bin/mount" []
--
-- Example: Access both @stdin@ and @stdout@ of an external program.
--
-- >import HsShellScript
-- >
-- >main = mainwrapper $ do
-- >
-- >   res <- pipe_from $
-- >      pipe_to "2\n3\n1" $
-- >         exec "/usr/bin/sort" []
-- >
-- >   putStrLn res
--
-- See 'exec', 'pipe_to', 'pipe_from', 'h_pipe_from2', 'lazy_pipe_from2', 'silently'.
-- See "HsShellScript#fdpipes" for more details.

pipe_from2 :: IO a              -- ^ Action to run as a separate process
           -> IO String         -- ^ The action's standard error output
pipe_from2 io = do
   (h, pid) <- h_pipe_from2 io
   txt <- hGetContents h
   seq (length txt) (hClose h)
   (Just ps) <- System.Posix.getProcessStatus True False pid
   if ps == Exited ExitSuccess
       then return txt
       else throw ps


-- | Run an IO action as a separate process, and connect to its @stdout@
-- with a file handle.
-- This is like the backquote feature of shells.
--
-- This forks a subprocess, which executes the specified action. A file handle,
-- which is connected to its @stdout@, is returned. The child's @ProcessID@
-- is returned as well. If the action replaces the child process, by calling an
-- @exec@ variant, it is made sure that its file descriptor 1 is connected to
-- the returned file handle.
--
-- This gives you full control of the pipe, and of the forked process. But you
-- need to deal with the child process by yourself.
--
-- When you call @getProcessStatus@ blockingly, you must first ensure that all
-- data has been read, or close the handle. Otherwise you'll get a deadlock.
-- When you close the handle before all data has been read, then the child gets
-- a @SIGPIPE@ signal.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally.
-- The child process is then properly terminated, such
-- that no resources, which have been duplicated by the fork, cause problems.
-- See "HsShellScript#subr" for details.
--
-- Errors can only be detected by examining the child's process status (using
-- 'System.Posix.Process.getProcessStatus'). No attempt is made to create more
-- meaningful exceptions, like it is done by @runprog@/@subproc@. If the child
-- action throws an exception, an error message is printed on @stderr@, and the
-- child process exits with a @ProcessStatus@ of @Exited (ExitFailure 1)@. The
-- following exceptions are understood, and result in corresponding messages:
-- @ArgError@, @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other
-- exceptions result in the generic message, as produced by @show@.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from stdin, is converted from the system character set to
-- Unicode. The system character set is determined by the environment variable
-- @LANG@. If you need to read binary data from the forked process, you can set
-- the returned handle to binary mode. This is accomplished by @'hSetBinaryMode'
-- h True@.
--
-- Example:
--
-- >(h,pid) <- h_pipe_from $ exec "/usr/bin/foo" ["bar"]
--
-- See 'exec', 'pipe_to', 'h_pipe_from2', 'pipe_from', 'lazy_pipe_from', 'HsShellScript.Misc.chomp', 'silently'.
-- See "HsShellScript#fdpipes" for more details.
h_pipe_from :: IO a                             -- ^ Action to run as a separate process, and to pipe from
            -> IO (Handle, ProcessID)           -- ^ Returns handle connected to the standard output of the child
                                                -- process, and the child's process ID
h_pipe_from io = do
   (_, Just h, _, pid) <- pipe_fork_dup io False True False
   return (h, pid)


-- | Run an IO action as a separate process, and connect to its @stderr@
-- with a file handle.
--
-- This forks a subprocess, which executes the specified action. A file handle,
-- which is connected to its @stderr@, is returned. The child's @ProcessID@
-- is returned as well. If the action replaces the child process, by calling an
-- @exec@ variant, it is made sure that its file descriptor 2 is connected to
-- the returned file handle.
--
-- This gives you full control of the pipe, and of the forked process. But you
-- need to deal with the child process by yourself.
--
-- When you call @getProcessStatus@ blockingly, you must first ensure that all
-- data has been read, or close the handle. Otherwise you'll get a deadlock.
-- When you close the handle before all data has been read, then the child gets
-- a @SIGPIPE@ signal.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally. The child process is
-- then properly terminated, such that no resources, which have been duplicated
-- by the fork, cause problems. See "HsShellScript#subr" for details.
--
-- Errors can only be detected by examining the child's process status (using
-- 'System.Posix.Process.getProcessStatus'). No attempt is made to create more
-- meaningful exceptions, like it is done by @runprog@/@subproc@. If the child
-- action throws an exception, an error message is printed on @stderr@. This
-- means that the message goes through the pipe to the parent process. Then the
-- child process exits with a @ProcessStatus@ of @Exited (ExitFailure 1)@. The
-- following exceptions are understood, and result in corresponding messages:
-- @ArgError@, @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other
-- exceptions result in the generic message, as produced by @show@.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from stdin, is converted from the system character set to
-- Unicode. The system character set is determined by the environment variable
-- @LANG@. If you need to read binary data from the forked process, you can set
-- the returned handle to binary mode. This is accomplished by @'hSetBinaryMode'
-- h True@.
--
-- Example:
--
-- >(h,pid) <- h_pipe_from $ exec "/usr/bin/foo" ["bar"]
--
-- See 'exec', 'pipe_from', 'pipe_from2', 'h_pipe_from', 'pipe_to',
-- 'lazy_pipe_from', 'HsShellScript.Misc.chomp', 'silently'.
h_pipe_from2 :: IO a                             -- ^ Action to run as a separate process, and to pipe from
             -> IO (Handle, ProcessID)           -- ^ Returns handle connected to the standard output of the child
                                                 -- process, and the child's process ID
h_pipe_from2 io = do
   (_, _, Just h, pid) <- pipe_fork_dup io False False True
   return (h, pid)




-- | Run an IO action in a separate process, and read its standard output, The output
-- is read lazily, as the returned string is evaluated. The child's output along
-- with its process ID are returned.
--
-- This forks a child process, which executes the specified action. The output
-- of the child is read lazily through a pipe, which connncts to its standard
-- output. In case the child replaces the process by calling an @exec@ variant,
-- it is make sure that the output is read from the new process' file descriptor
-- 1.
--
-- @lazy_pipe_from@ calls 'System.IO.hGetContents', in order to read the pipe
-- lazily. This means that the file handle goes to semi-closed state. The handle
-- holds a file descriptor, and as long as the string isn't fully evaluated,
-- this file descriptor won't be closed. For the file descriptor to be closed,
-- first its standard output needs to be closed on the child side. This happens
-- when the child explicitly closes it, or the child process exits. When
-- afterwards the string on the parent side is completely evaluated, the handle,
-- along with the file descritor it holds, are closed and freed.
--
-- If you use the string in such a way that you only access the beginning of the
-- string, the handle will remain in semi-closed state, holding a file
-- descriptor, even when the pipe is closed on the child side. When you do that
-- repeatedly, you may run out of file descriptors.
--
-- Unless you're sure that your program will reach the string's end, you should
-- take care for it explicitly, by doing something like this:
--
-- >(output, pid) <- lazy_pipe_from (exec "\/usr\/bin\/foobar" [])
-- >...
-- >seq (length output) (return ())
--
-- This will read the entire standard output of the child, even if it isn't
-- needed. You can't cut the child process' output short, when you use
-- @lazy_pipe_from@. If you need to do this, you should use @h_pipe_from@, which
-- gives you the handle, which can then be closed by 'System.IO.hClose', even
-- if the child's output isn't completed:
--
-- >(h, pid) <- h_pipe_from io
-- >
-- >-- Lazily read io's output
-- >output <- hGetContents h
-- >...
-- >-- Not eveyting read yet, but cut io short.
-- >hClose h
-- >
-- >-- Wait for io to finish, and detect errors
-- >(Just ps) <- System.Posix.getProcessStatus True False pid
-- >when (ps /= Exited ExitSuccess) $
-- >   throw ps
--
-- When you close the handle before all data has been read, then the child gets
-- a @SIGPIPE@ signal.
--
-- After all the output has been read, you should call @getProcessStatus@ on the
-- child's process ID, in order to detect errors. Be aware that you must
-- evaluate the whole string, before calling @getProcessStatus@ blockingly, or
-- you'll get a deadlock.
--
-- You won't get an exception, if the child action exits in a way which
-- indicates an error. Errors occur asynchronously, when the output string is
-- evaluated. You must detect errors by yourself, by calling
-- 'System.Posix.Process.getProcessStatus'.
--
-- In case the action doesn't replace the child process with an external
-- program, an exception may be thrown out of the action. This results in an error
-- message on @stderr@, and the proper termination of the child. The
-- @ProcessStatus@, which can be accessed in the parent process by
-- @getProcessStatus@, is @Exited (ExitFailure 1)@. The following exceptions are
-- understood, and result in corresponding messages: @ArgError@,
-- @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other exceptions
-- result in the generic message, as produced by @show@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally. The child process is
-- then properly terminated, such that no resources, which have been duplicated
-- by the fork, cause problems. See "HsShellScript#subr" for details.
--
-- Unlike shells\' backquote feature, @lazy_pipe_from@ does not remove any trailing
-- newline characters. The entire output of the action is returned. You might want
-- to apply @chomp@ to the result.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from the IO action's stdout, are converted from the system
-- character set to Unicode. The system character set is determined by the
-- environment variable @LANG@. If you need to read binary data from the forked
-- process, you should use h_pipe_from and set the returned handle to binary mode.
-- This is accomplished by @'hSetBinaryMode' h True@. Then you can lazily read 
-- the output of the action from the handle.
--
-- Example: Lazily read binary data from an IO action. Don't forget to collect 
-- the child process later, using @'System.Posix.getProcessStatus' True False pid@.
--
-- >(h, pid) <- h_pipe_from io
-- >hSetBinaryMode h True
-- >txt <- hGetContents h
-- >...
-- >(Just ps) <- System.Posix.getProcessStatus True False pid
--
-- See 'exec', 'pipe_to', 'pipe_from', 'h_pipe_from', 'lazy_pipe_from2', 'silently'.
lazy_pipe_from :: IO a                          -- ^ Action to run as a separate process
               -> IO (String, ProcessID)        -- ^ The action's lazy output and the process ID of the child
                                                -- process
lazy_pipe_from io = do
   (_, Just h, _, pid) <- pipe_fork_dup io False True False
   txt <- hGetContents h
   return (txt, pid)


-- | Run an IO action in a separate process, and read its standard error output, The output
-- is read lazily, as the returned string is evaluated. The child's error output along
-- with its process ID are returned.
--
-- This forks a child process, which executes the specified action. The error output
-- of the child is read lazily through a pipe, which connncts to its standard error
-- output. In case the child replaces the process by calling an @exec@ variant,
-- it is make sure that the output is read from the new process' file descriptor
-- 1.
--
-- @lazy_pipe_from@ calls 'System.IO.hGetContents', in order to read the pipe
-- lazily. This means that the file handle goes to semi-closed state. The handle
-- holds a file descriptor, and as long as the string isn't fully evaluated,
-- this file descriptor won't be closed. For the file descriptor to be closed,
-- first its standard error output needs to be closed on the child side. This happens
-- when the child explicitly closes it, or the child process exits. When
-- afterwards the string on the parent side is completely evaluated, the handle,
-- along with the file descritor it holds, are closed and freed.
--
-- If you use the string in such a way that you only access the beginning of the
-- string, the handle will remain in semi-closed state, holding a file
-- descriptor, even when the pipe is closed on the child side. When you do that
-- repeatedly, you may run out of file descriptors.
--
-- Unless you're sure that your program will reach the string's end, you should
-- take care for it explicitly, by doing something like this:
--
-- >(errmsg, pid) <- lazy_pipe_from2 (exec "/usr/bin/foobar" [])
-- >...
-- >seq (length errmsg) (return ())
--
-- This will read the entire standard error output of the child, even if it isn't
-- needed. You can't cut the child process' output short, when you use
-- @lazy_pipe_from@. If you need to do this, you should use @h_pipe_from@, which
-- gives you the handle, which can then be closed by 'System.IO.hClose', even
-- if the child's output isn't completed:
--
-- >(h, pid) <- h_pipe_from io
-- >
-- >-- Lazily read io's output
-- >output <- hGetContents h
-- >...
-- >-- Not eveyting read yet, but cut io short.
-- >hClose h
-- >
-- >-- Wait for io to finish, and detect errors
-- >(Just ps) <- System.Posix.getProcessStatus True False pid
-- >when (ps /= Exited ExitSuccess) $
-- >   throw ps
--
-- When you close the handle before all data has been read, then the child gets
-- a @SIGPIPE@ signal.
--
-- After all the output has been read, you should call @getProcessStatus@ on the
-- child's process ID, in order to detect errors. Be aware that you must
-- evaluate the whole string, before calling @getProcessStatus@ blockingly, or
-- you'll get a deadlock.
--
-- You won't get an exception, if the child action exits in a way which
-- indicates an error. Errors occur asynchronously, when the output string is
-- evaluated. You must detect errors by yourself, by calling
-- 'System.Posix.Process.getProcessStatus'.
--
-- In case the action doesn't replace the child process with an external
-- program, an exception may be thrown out of the action. This results in an
-- error message on @stderr@. This means that the message is sent through the
-- pipe, to the parent process. Then the child process is properly terminated.
-- The @ProcessStatus@, which can be accessed in the parent process by
-- @getProcessStatus@, is @Exited (ExitFailure 1)@. The following exceptions are
-- understood, and result in corresponding messages: @ArgError@,
-- @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other exceptions
-- result in the generic message, as produced by @show@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally. The child process is
-- then properly terminated, such that no resources, which have been duplicated
-- by the fork, cause problems. See "HsShellScript#subr" for details.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from stdin, is converted from the system character set to
-- Unicode. The system character set is determined by the environment variable
-- @LANG@. If you need to read binary data from the forked process, you can set
-- the returned handle to binary mode. This is accomplished by @'hSetBinaryMode'
-- h True@.
--
-- Unlike shells\' backquote feature, @lazy_pipe_from@ does not remove any trailing
-- newline characters. The entire output of the action is returned. You might want
-- to apply @chomp@ to the result.
--
-- The pipe is set to /text mode/. This means that the Unicode characters in the
-- text, which is read from the IO action's stdout, are converted from the
-- system character set to Unicode. The system character set is determined by
-- the environment variable @LANG@. If you need to read binary data from the
-- forked process' standard error output, you should use h_pipe_from2 and set
-- the returned handle to binary mode. This is accomplished by @'hSetBinaryMode'
-- h True@. Then you can lazily read the output of the action from the handle.
--
-- Example: Lazily read binary data from an IO action. Don't forget to collect 
-- the child process later, using @'System.Posix.getProcessStatus' True False pid@.
--
-- >(h, pid) <- h_pipe_from2 io
-- >hSetBinaryMode h True
-- >txt <- hGetContents h
-- >...
-- >(Just ps) <- System.Posix.getProcessStatus True False pid
--
-- See 'exec', 'pipe_to', 'pipe_from2', 'h_pipe_from2', 'lazy_pipe_from', 'silently'.
lazy_pipe_from2 :: IO a                          -- ^ Action to run as a separate process
                -> IO (String, ProcessID)        -- ^ The action's lazy output and the process ID of the child
                                                 -- process
lazy_pipe_from2 io = do
   (_, _, Just h, pid) <- pipe_fork_dup io False False True
   txt <- hGetContents h
   return (txt, pid)



-- | Run an IO action as a separate process, and read its @stdout@ strictly. All
-- the output is read, until the action terminates. Contrary to 'pipe_from',
-- when the action signals an error (with a non-zero exit code), the error isn't
-- thrown as an exception, but is returned alongside the output.
--
-- The result consists of the output which has been read, alongside with the
-- process status, with which the action has terminated. For success,
-- the process status is @Exited ExitSuccess@. See
-- 'System.Posix.Process.ProcessStatus'.
--
-- This is a frontend to the 'h_pipe_from' function. See there for more details.
--
-- See 'pipe_from_full2', 'exec', 'pipe_to', 'pipe_from', 'h_pipe_from', 'lazy_pipe_from', 'HsShellScript.Misc.chomp'.

pipe_from_full
  :: IO a                       -- ^ Action to run as a separate process. Its return value is ignored.
  -> IO (String, ProcessStatus) -- ^ The output of the IO action until it terminated and
                                -- the process status of the terminated action.

pipe_from_full io = do
   (h, pid) <- h_pipe_from io
   txt <- hGetContents h
   seq (length txt) (hClose h)
   (Just ps) <- System.Posix.getProcessStatus True False pid
   return (txt, ps)


-- | Run an IO action as a separate process, and read its @stderr@ strictly. All
-- the error output is read, until the action terminates. Contrary to 'pipe_from2',
-- when the action signals an error (with a non-zero exit code), the error isn't
-- thrown as an exception, but is returned alongside the output.
--
-- The result consists of the error output which has been read, alongside with
-- the process status, with which the action has terminated. For success, the
-- process status is @Exited ExitSuccess@. See
-- 'System.Posix.Process.ProcessStatus'.
--
-- This is a frontend to the 'h_pipe_from2' function. See there for more details.
--
-- See 'pipe_from_full', 'exec', 'pipe_to', 'pipe_from', 'h_pipe_from', 'lazy_pipe_from',
-- 'HsShellScript.Misc.chomp'.

pipe_from_full2
  :: IO a                       -- ^ Action to run as a separate process. Its return value is ignored.
  -> IO (String, ProcessStatus) -- ^ The error output of the IO action until it terminated and
                                -- the process status of the terminated action.
pipe_from_full2 io = do
   (h, pid) <- h_pipe_from2 io
   txt <- hGetContents h
   seq (length txt) (hClose h)
   (Just ps) <- System.Posix.getProcessStatus True False pid
   return (txt, ps)




-- | Run an IO action as a separate process, and optionally connect to its
-- @stdin@, its @stdout@ and its @stderr@ output with pipes.
--
-- This forks a subprocess, which executes the specified action. The child\'s
-- @ProcessID@ is returned. Some of the action\'s standard handles are made to
-- connected to pipes, which the caller can use in order to communicate with the
-- new child process. Which, this is determined by the first three arguments.
--   
-- You get full control of the pipes, and of the forked process. But you
-- need to deal with the child process by yourself.
--
-- Errors in the child process can only be detected by examining its process
-- status (using 'System.Posix.Process.getProcessStatus'). If the child action
-- throws an exception, an error message is printed on @stderr@, and the child
-- process exits with a @ProcessStatus@ of @Exited (ExitFailure 1)@. The
-- following exceptions are understood, and result in corresponding messages:
-- @ArgError@, @ProcessStatus@, @RunError@, @IOError@ and @ExitCode@. Other
-- exceptions result in the generic message, as produced by @show@.
--
-- Unless you replace the child process, calling an @exec@ variant, the child
-- should let the control flow leave the action normally. It is then properly 
-- take care of.
--
-- The pipes are set to /text mode/. When connecting to the child\'s @stdin@,
-- this means that the Unicode characters in the Haskell side text are converted
-- to the system character set. When reading from the child\'s @stdout@ or
-- @stderr@, the text is converted from the system character set to Unicode in
-- the Haskell-side strings. The system character set is determined by the
-- environment variable @LANG@. If you need to read or write binary data, then
-- this is no problem. Just call @'hSetBinaryMode' handle True@. This sets the
-- corresponding pipe to binary mode.
--
-- See 'pipe_from', 'h_pipe_from', 'pipe_from2', 'h_pipe_from2', 'pipe_to', 
-- 'h_pipe_to', 'lazy_pipe_from', 'lazy_pipe_from2'
pipes :: IO a                   -- ^ Action to run in a new process
      -> Bool                   -- ^ Whether to make stdin pipe
      -> Bool                   -- ^ Whether to make stdout pipe
      -> Bool                   -- ^ Whether to make stderr pipe
      -> IO ( Maybe Handle
            , Maybe Handle
            , Maybe Handle
            , ProcessID
            )                   -- ^ Pipes to the new process's @stdin@, @stdout@ and @stderr@, if applicable;
                                -- and its process id.
pipes = pipe_fork_dup


-- {- | Execute the supplied action. In case of an error, exit with an error
-- message.
--
-- > Noch nicht auf neue Exception-Bibliothek portiert. <
--
-- An error is an exception, thrown using @throw@ as a type which is
-- instance of @Typeable@. The type err is supposed to be a specific type used
-- for specific errors. The program is terminated with @exitFailure@.
-- -}
-- abort :: Exception err
--       => (err -> String)        -- ^ Error message generation function
--       -> IO a                   -- ^ IO action to monitor
--       -> IO a                   -- ^ Same action, but abort with error message in case of user exception
-- abort msgf io =
--    io
--    `catch` (\se -> hPutStrLn stderr (msgf errval) >> exitFailure)


{- | Forcibly terminate the program, circumventing normal program shutdown.

This is the @_exit(2)@ system call. No cleanup actions installed with @bracket@
are performed, no data buffered by file handles is written out, etc.
-}
_exit :: Int                    -- ^ Exit code
      -> IO a                   -- ^ Never returns
_exit ec = do
   {#call _exit as _exit_prim#} (fromIntegral ec)
   error "Impossible error" -- never reached, only for the type checker



-- | Generate an error message from an @errno@ value. This is the POSIX
-- @strerror@ system library function.
--
-- See the man page @strerror(3)@.
strerror :: Errno       -- ^ @errno@ value
         -> IO String   -- ^ Corresponding error message
strerror (Errno errno) = do
    peekCString ({#call pure strerror as foreign_strerror#} errno)


-- | Read the global system error number. This is the POSIX @errno@ value. This
-- function is redundant. Use @Foreign.C.Error.getErrno@ instead.
errno :: IO Errno       -- ^ @errno@ value
errno = getErrno


-- | Print error message corresponding to the specified @errno@ error
-- number. This is similar to the POSIX system library function @perror@.
--
-- See the man page @perror(3)@.
perror' :: Errno        -- ^ @errno@ error number
        -> String       -- ^ Text to precede the message, separated by \"@: @\"
        -> IO ()
perror' errno txt = do
   str <- strerror errno
   hPutStrLn stderr ((if txt == "" then "" else txt ++ ": ") ++ str)


-- | Print error message corresponding to the global @errno@ error
-- number. This is the same as the POSIX system library function @perror@.
--
-- See the man page @perror(3)@.
perror :: String        -- ^ Text to precede the message, separated by \"@: @\"
       -> IO ()
perror txt = do
   eno <- getErrno
   perror' eno txt


-- | Print a message to @stderr@ and exit with an exit code
-- indicating an error.
--
-- >failIO msg = hPutStrLn stderr msg >> exitFailure
failIO :: String -> IO a
failIO meld =
   hPutStrLn stderr meld >> exitFailure


-- | Modify an IO action to return the exit code of a failed program call,
-- instead of throwing an exception.
--
-- This is used to modify the error reporting behaviour of an IO action which
-- uses 'run'/'runprog' or 'call'/'subproc'. When an external program exits with
-- an exit code which indicates an error, normally an exception is thrown. After
-- @exitcode@ has been applied, the exit code is retruned instead.
--
-- The caught exceptions are 'RunError' and 'ProcessStatus'. Termination by a
-- signal is still reported by an exception, which is passed through.
--
-- Example: @ec \<- exitcode $ runprog \"foo\" [\"bar\"]@
--
-- See 'runprog', 'subproc', 'run', 'call'.
exitcode :: IO ()             -- ^ Action to modify
         -> IO ExitCode       -- ^ Modified action
exitcode io =
   do io
      return ExitSuccess
   `catch`
      (\processstatus ->
          case processstatus of
             (Exited ec) -> return ec
             ps          -> throw ps)
   `catch`
      (\re ->
          case re_ps re of
             (Exited ec) -> return ec
             ps          -> throw re)


-- |Create and throw an @IOError@ from the current @errno@ value, an optional handle and an optional file name.
--
-- This is an extended version of the @Foreign.C.Error.throwErrno@ function
-- from the GHC libraries, which additionally allows to specify a handle and a file
-- name to include in the @IOError@ thrown.
--
-- See @Foreign.C.Error.throwErrno@, @Foreign.C.Error.errnoToIOError@.
throwErrno' :: String           -- ^ Description of the location where the error occurs in the program
            -> Maybe Handle     -- ^ Optional handle
            -> Maybe FilePath   -- ^ Optional file name (for failing operations on files)
            -> IO a
throwErrno' loc maybe_handle maybe_filename =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno maybe_handle maybe_filename)


-- |Convert an @IOError@ to a string.
--
-- There is an instance declaration of @IOError@ in @Show@ in the @GHC.IO@ library, but @show_ioerror@ produces a
-- more readable, and more complete, message.
show_ioerror :: IOError -> String
show_ioerror ioe =
   "IO-Error\n\
   \   Error type:   " ++ show (ioeGetErrorType ioe) ++ "\n\
   \   Location:     " ++ none (indent (ioe_location ioe)) ++ "\n\
   \   Description:  " ++ none (indent (ioe_description ioe)) ++ "\n\
   \   " ++ fn (ioeGetFileName ioe)
   where fn (Just n) = "File name:    " ++ quote n
         fn Nothing  = "File name:    (none)"
         none ""  = "(none)"
         none msg = msg
         indent txt = concat (intersperse ("\n                 ") (lines txt))


{- | Call the shell to execute a command. In case of an error, throw the @ProcessStatus@ (such as @(Exited
(ExitFailure ec))@) as an exception. This is like the Haskell standard library function @system@, except that error
handling is brought in accordance with HsShellScript\'s scheme.

@exitcode . system_throw@ is the same as the @system@ function, except that when the called shell is terminated or
stopped by a signal, this still lead to the @ProcessStatus@ being thrown. The Haskell library report says nothing
about what happens in this case, when using the @system@ function.

>system_throw cmd = run "/bin/sh" ["-c", "--", cmd]

This function is deprecated. You should rather use 'system_runprog', which provides for much better error
reporting.
-}
-- This function should go to HsShellScript.Shell, but this would introduce a circular dependency.
system_throw :: String -> IO ()
system_throw cmd =
   run "/bin/sh" ["-c", "--", cmd]




{- | Call the shell to execute a command. In case of an error, a @RunError@ ist thrown. This is like the Haskell
standard library function @system@, except that error handling is brought in accordance with HsShellScript's
scheme. (It is /not/ a front end to @system@.)

>system_runprog cmd = runprog "/bin/sh" ["-c", "--", cmd]

Example: Call \"foo\" and report Errors as @IOError@s, rather than @RunError@s.

>as_ioe $ system_runprog "foo" ["bar", "baz"]

See 'RunError', 'as_ioe'
-}

-- This function should go to HsShellScript.Shell, but this would introduce a circular dependency.
system_runprog :: String -> IO ()
system_runprog cmd =
   runprog "/bin/sh" ["-c", "--", cmd]



{- | Run a subroutine as a child process, but don't let it produce any messages.
Read its @stdout@ and @stderr@ instead, and append it to the contents of a
mutable variable. The idea is that you can run some commands silently, and
report them and their messages to the user only when something goes wrong.

If the child process terminates in a way which indicates an error, then the
process status is thrown, in the same way as 'runprog' does. If the subroutine
throws an @(Exited ec)@ exception (of type @ProcessStatus@), such as thrown by
'runprog', then the child process exits with the same exit code, such that the
parent process reports it to the caller, again as a @ProcessStatus@ exception.

When the subroutine finishes, the child process is terminated with @'_exit' 0@.
When it throws an exception, an error message is printed and it is terminated
with @'_exit' 1@. See "HsShellScript#subr" for details.

The standard output (and the standard error output) of the parent process are
flushed before the fork, such that no output appears twice.

Example:

>let handler :: IORef String -> ProcessStatus -> IO ()
>    handler msgref ps = do hPutStrLn stderr ("Command failed with " ++ show ps ++ ". Actions so far: ")
>                           msg <- readIORef msgref
>                           hPutStrLn stderr msg
>                           exitWith (ExitFailure 1)
>
>msgref <- newIORef ""
>do silently msgref $ do putStrLn "Now doing foobar:"
>                        echo exec "/foo/bar" ["arguments"]
>   silently msgref $ echo exec "/bar/baz" ["arguments"]
>`catch` (handler msgref)

See 'lazy_pipe_from', 'subproc', 'runprog', Data.IORef.
-}
silently :: IORef.IORef String       -- ^ A mutable variable, which gets the output (stdout and stderr) of the
                                     -- action appended.
         -> IO ()                    -- ^ The IO action to run.
         -> IO ()
silently ref io = do
   (msg, pid) <- lazy_pipe_from (err_to_out (child io))
   seq (length msg) (return ())

   msgs <- readIORef ref
   writeIORef ref (msgs ++ msg)

   (Just ps) <- getProcessStatus True False pid
   case ps of
      Exited ExitSuccess -> return ()
      ps                 -> throw ps


{- | Modify a subroutine action in order to make it suitable to run as a child
   process.

   This is used by functions like 'call', 'silently', 'pipe_to' etc. The action
   is executed. When it returns, the (child) process is terminated with @'_exit' 0@
   (after flushing @stdout@), circumventing normal program shutdown. When it
   throws an exception, an error message is printed and the (child) process is
   terminated with @'_exit' 1@.
-}
child :: IO a           -- Action to modify
      -> IO b           -- Never returns
child io = do
   io
      `catches`
      [ Handler $ (\argerror -> do
                      errm $ "In child process:\n" ++ argerror_message argerror
                      flush_outerr
                      _exit 1
                  )
      , Handler $ (\processstatus -> do
                      errm $ "Process error in child process. Process status = " ++
                             show ( processstatus :: ProcessStatus )
                      flush_outerr
                      _exit 1
                  )
      , Handler $ (\(runerror::RunError) -> do
                      errm $ (show_runerror runerror)
                      flush_outerr
                      _exit 1
                  )
      , Handler $ (\(ioe::IOError) -> do
                      errm ("In child process:\n   " ++ show_ioerror ioe)
                      flush_outerr
                      _exit 1
                  )
      , Handler $ (\(e::ExitCode) -> do
                      -- Child process is a subroutine that has terminated normally.
                      let ec = case e of
                                  ExitSuccess     -> 0
                                  ExitFailure ec' -> ec'
                      errm ("Warning! Child process tries to shut down normally. This is a bug. It should\n\
                            \terminate with _exit (or catch the ExitException yourself). See section\n\"\
                            \Running a Subroutine in a Separate Process\" in the HsShellScript API\n\
                            \documentation. Terminating with _exit " ++ show ec ++ " now.")
                      flush_outerr
                      _exit ec)
      , Handler $ (\(e::SomeException) -> do
                     errm ("Child process quit with unexpected exception:\n" ++ show e)
                     flush_outerr
                     _exit 1
                  )
      ]

   flush_outerr
   _exit 0


{- | Print text to @stdout@.

   This is a shorthand for @putStrLn@, except for @stderr@ being flushed
   beforehand. This way normal output and error output appear in
   order, even when they aren't buffered as by default.

   An additional newline is printed at the end.

   >outm msg = do
   >   hFlush stderr
   >   putStrLn msg
-}
outm :: String          -- ^ Message to print
     -> IO ()
outm msg = do
   hFlush stderr
   putStrLn msg


{- | Print text to @stdout@.

   This is a shorthand for @putStr@, except for @stderr@ being flushed
   beforehand. This way normal output and error output appear in
   order, even when they aren't buffered as by default.

   No newline is printed at the end.

   >outm_ msg = do
   >   hFlush stderr
   >   putStr msg
-}
outm_ :: String          -- ^ Message to print
      -> IO ()
outm_ msg = do
   hFlush stderr
   putStr msg


{- | Colorful log message to @stderr@.

   This prints a message to @stderr@. When @stderr@ is connected to a terminal
   (as determined by @isatty(3)@), additional escape sequences are printed,
   which make the message appear in cyan. Additionally, a newline character is
   output at the end.

   @stdout@ is flushed beforehand. So normal output and error output appear in
   order, even when they aren't buffered as by default.

   See 'logm_', 'errm', 'errm_'.
-}
logm :: String          -- ^ Message to print
     -> IO ()
logm msg =
   do hFlush stdout
      tty <- isatty stderr
      if tty
         then hPutStrLn stderr $ "\ESC[36m" ++ msg ++ "\ESC[00m"
         else hPutStrLn stderr msg


{- | Colorful log message to @stderr@.

   This prints a message to @stderr@. When @stderr@ is connected to a terminal
   (as determined by @isatty(3)@), additional escape sequences are printed,
   which make the message appear in cyan. No a newline character is output at the end.

   @stdout@ is flushed beforehand. So normal output and error output appear in
   order, even when they aren't buffered as by default.

   See 'logm', 'errm', 'errm_'.
-}
logm_ :: String -> IO ()
logm_ msg = do
   do hFlush stdout
      tty <- isatty stderr
      if tty
         then hPutStr stderr $ "\ESC[36m" ++ msg ++ "\ESC[00m"
         else hPutStr stderr msg


{- | Colorful error message to @stderr@.

   This prints a message to @stderr@. When @stderr@ is connected to a terminal
   (as determined by @isatty(3)@), additional escape sequences are printed,
   which make the message appear in red. Additionally, a newline character is
   output at the end.

   @stdout@ is flushed beforehand. So normal output and error output appear in
   order, even when they aren't buffered as by default.

   See 'logm', 'logm_', 'errm_'.
-}
errm :: String -> IO ()
errm msg = do
   do hFlush stdout
      tty <- isatty stderr
      if tty
         then hPutStrLn stderr $ "\ESC[01;31m" ++ msg ++ "\ESC[00m"
         else hPutStrLn stderr msg


{- | Colorful error message to @stderr@.

   This prints a message to @stderr@. When @stderr@ is connected to a terminal
   (as determined by @isatty(3)@), additional escape sequences are printed,
   which make the message appear in red. No a newline character is output at the end.

   @stdout@ is flushed beforehand. So normal output and error output appear in
   order, even when they aren't buffered as by default.

   See 'logm', 'logm_', 'errm'.
-}
errm_ :: String -> IO ()
errm_ msg = do
   do hFlush stdout
      tty <- isatty stderr
      if tty
         then hPutStr stderr $ "\ESC[01;31m" ++ msg ++ "\ESC[00m"
         else hPutStr stderr msg


{- | In case the specified action throws an IOError, fill in its filename field. This way, more useful error
messages can be produced.

Example:

>-- Oh, the GHC libraries neglect to fill in the file name
>executeFile' prog a b c =
>   fill_in_filename prog $ executeFile prog a b c

See 'fill_in_location', 'add_location'.
-}
fill_in_filename :: String              -- ^ File name to fill in
                 -> IO a                -- ^ IO action to modify
                 -> IO a                -- ^ Modified IO action
fill_in_filename filename io =
   io `catch` (\ioe -> ioError (ioeSetFileName ioe filename))


{- | In case the specified action throws an IOError, fill in its location field. This way, more useful error
messages can be produced.

Example:

>my_fun a b c = do
>   -- ...
>   fill_in_location "my_fun" $  -- Give the caller a more useful location information in case of failure
>      rename "foo" "bar"
>   -- ...

See 'fill_in_filename'.
-}
fill_in_location :: String              -- ^ Location name to fill in
                 -> IO a                -- ^ IO action to modify
                 -> IO a                -- ^ Modified IO action
fill_in_location location io =
   io `catch` (\ioe -> ioError (ioeSetLocation ioe location))


{- | In case the specified action throws an IOError, add a line to its location field. This way, more useful
error messages can be produced. The specified string is prepended to the old location, separating it with a
newline from the previous location, if any. When using this thoroughly, you get a reverse call stack in
IOErrors.

Example:

>my_fun =
>   add_location "my_fun" $ do
>      -- ...

See 'fill_in_filename', 'fill_in_location'.
-}
add_location :: String              -- ^ Location name to add
             -> IO a                -- ^ IO action to modify
             -> IO a                -- ^ Modified IO action
add_location location io =
   io `catch` (\ioe -> let loc = case ioe_location ioe of
                                    ""   -> location
                                    loc0 -> location ++ "\n" ++ loc0
                       in  ioError (ioe { ioe_location = loc })
              )


{- | This is a replacement for @System.Posix.Process.executeFile@. It does
   additional preparations, then calls @executeFile@. @executeFile@ /can't normally/
   /be used directly, because it doesn't do the things which are/
   /outlined here./

   This are the differences to @executeFile@:

   1. @stdout@ and @stderr@ are flushed.

   2. The standard file descriptors 0-2 are made copies of the file descriptors
   which the standard handles currently use. This is necessary because they
   might no longer use the standard handles. See "HsShellScript#fdpipes".

   If the standard handles @stdin@, @stdout@, @stderr@ aren't in closed state,
   and they aren't already connected to the respective standard file
   descriptors, their file descriptors are copied to the respective standard
   file descriptors (with @dup2@). Backup copies are made of the file
   descriptors which are overwritten. If some of the standard handles are closed,
   the corresponding standard file descriptors are closed as well.

   3. All file descriptors, except for the standard ones, are set to close-on-exec
   (see @fcntl(2)@), and will be closed on successful replacement of
   the process. Before that, the old file descriptor flags are saved.

   4. The standard file descriptors are set to blocking mode, since GHC 6.2.2
   sets file descriptors to non-blocking (except 0-2, which may get
   overwritten by a non-blocking one in step 2). The called program
   doesn't expect that.

   5. In case replacing the process fails, the file descriptors are reset to
   the original state. The file descriptors flags are restored, and the file
   descriptors 0-2 are overwritten again, with their backup copies. Then an
   IOError is thrown.

   6. In any IOError, the program is filled in as the file name (@executeFile@
   neglects this).

   7. The return type is a generic @a@, rather than @()@.

   Also see "HsShellScript#exec".
-}
execute_file :: FilePath                     -- ^ Program to call
             -> Bool                         -- ^ Search @PATH@?
             -> [String]                     -- ^ Arguments
             -> Maybe [(String, String)]     -- ^ Optionally new environment
             -> IO a                         -- ^ Never returns
execute_file path search args menv =
   fill_in_filename path $ fill_in_location "execute_file" $ do
      bracket
         (do -- Flush stdout and stderr, if open
             flush_outerr

             -- Make fds 0-2 copies of the things which the standard handles refer to.
             recover0 <- restore stdin 0
             recover1 <- restore stdout 1
             recover2 <- restore stderr 2

             -- Save the flags of all file descriptors
             fdflags <- {# call c_save_fdflags #}

             -- Prepare all fds for subsequent exec. Fds 0-2 are set to blocking (because GHC sets new fds to
             -- non-blocking). All others are set to close-on-exec.
             {# call c_prepare_fd_flags_for_exec #}

             return (recover0, recover1, recover2, fdflags)
         )
         (\(recover0, recover1, recover2, fdflags) ->
             do -- Failure of the exec. Restore the file descriptor flags
                {# call c_restore_fdflags #} fdflags

                -- Restore the standard handles
                recover0
                recover1
                recover2
         )
         (const $ do
             -- The exec. Throws an IOError in case replacing the process failed.
             executeFile path search args menv

             -- Never reached, only for the type checker
             error "Impossible error"
         )

   where
      restore h@(FileHandle _ mvar) fd = do
         -- The fd used by the handle. This is in GHC.IO.Handle.FD
         --                              handleToFd_noclose: Fehlerhaft, aus hssh-2.9
         -- handle_fd: der file descriptor, den der Handle mitbringt. Weicht möglicherweise von 0-2 ab.
         handle_fd <- fmap fromIntegral (handleToFd_noclose h)

         -- Get the fd which the handle h uses. This locks the handle.
         (h__ :: Handle__) <- takeMVar mvar

         -- Make a copy of the fd which is about to be overwritten. Returns -1 for invalid (closed) fd.
         -- Mache Sicherheitskopie des Standard-file descriptor (0-2) in einem neu zu belegenden f.d. (ab 3).
         -- fd: Standard-file descritor, 0-2.
         -- Bewegt den Standardfiledescriptor aus dem Weg.
         fd_backup <- {# call c_fcntl_dupfd #} fd 3
         -- Liefert den neuen file descriptor, oder -1 (bei Fehler), wenn der filedescriptor geschlossen ist


         -- Is the handle closed?
         let closed = case haType h__ of
                         ClosedHandle -> True
                         SemiClosedHandle -> True
                         otherwise -> False

         -- If the handle is open, make the fd a copy of the fd which the handle uses. Otherwise, close the fd
         -- as well.
         if closed
            then {# call close #} fd >> return ()
            else when (fd /= handle_fd) $
                 -- Den f.d., den der Standard-Handle benutzt, auf die Standardposition in 0-2 kopieren.
                 {# call dup2 #} handle_fd fd >> return ()


         -- Return recovery action which undoes everything.
         return (do -- Restore the fd
                    if fd_backup /= -1 then do -- Den Inhalt des 0-2-file descriptors wiederherstellen
                                               {# call dup2 #} fd_backup fd
                                               -- Die Sicherheitskopie wieder freigeben
                                               {# call close #} fd_backup

                                               return ()
                                       else do -- Wenn der 0-2-filedescriptor nicht kopiert werden konnte,
                                               -- dann liegt das (?) daran, daß er geschlossen war. Ihn dann
                                               -- wieder schließen.
                                               {# call close #} fd
                                               return ()
                    -- Unlock the handle
                    putMVar mvar h__
                    return ()
                )

      -- Silly: The standard handle has been overwritten with a duplex.
      restore h fd = do
        -- Make a copy of the fd which is about to be closed. Returns -1 for already closed fd.
        fd_backup <- {# call c_fcntl_dupfd #} fd 3

        -- Close the fd
        {# call close #} fd

        -- Return recovery action, which restores the fd.
        return (if fd_backup /= -1 then do {# call dup2 #} fd_backup fd
                                           {# call close #} fd_backup
                                           return ()
                                   else do {# call close #} fd
                                           return ()
               )



handleToFd_noclose :: Handle -> IO Fd
handleToFd_noclose h =
    unsafeWithHandleFd h (\fd -> return fd)



{- About Bas van Dijk's unsafeWithHandleFd:

   This function is broken. It blocks when called like this:

   -- Blocks
   unsafeWithHandleFd stdout $ \fd ->
      putStrLn ("stdout: fd = " ++ show fd)

   The job of unsafeWithHandleFd's job is, to keep a reference to
   the handle, so it won't be garbage collected, while the action is still
   running. Garbage collecting the handle would close it, as well as the
   underlying file descriptor, while the latter is still in use by the action.
   This can't happen as long as use of the file descriptor is encapsulated in the
   action.

   This encapsulation can be circumvented by returning the file descriptor, and
   that's what I do in execute_file. This should usually not be done.

   However, I want to use it on stdin, stdout and stderr, only. These three
   should never be garbage collected. Under this circumstances, it should be
   safe to use unsafeWithHandleFd this way.
-}

unsafeWithHandleFd :: Handle -> (Fd -> IO a) -> IO a
unsafeWithHandleFd h@(FileHandle _ m)     f = unsafeWithHandleFd' h m f
-- unsafeWithHandleFd h@(DuplexHandle _ _ w) f = unsafeWithHandleFd' h w f

unsafeWithHandleFd' :: Handle -> MVar Handle__ -> (Fd -> IO a) -> IO a
unsafeWithHandleFd' h m f =
  withHandle' "unsafeWithHandleFd" h m $ \h_@Handle__{haDevice} ->
    case cast haDevice of
      Nothing -> ioError (System.IO.Error.ioeSetErrorString (System.IO.Error.mkIOError IllegalOperation
                                                             "unsafeWithHandleFd" (Just h) Nothing)
                         "handle is not a file descriptor")
      Just fd -> do
        x <- f (Fd (FD.fdFD fd))
        return (h_, x)


-------------------------------------------------------------------------------------------------------------



{- | Check if a handle is connected to a terminal.

   This is a front end to the @isatty(3)@ function (see man page). It is useful,
   for instance, to determine if color escape sequences should be
   generated.
-}

isatty :: Handle        -- ^ Handle to check
       -> IO Bool       -- ^ Whether the handle is connected to a terminal
isatty h =
   unsafeWithHandleFd h $ \fd -> do
      isterm <- {# call isatty as hssh_c_isatty #} ((fromIntegral fd) :: CInt)
      return (isterm /= (0::CInt))


-- Flush stdout and stderr (which should not be necessary). Discard Illegal Operation IOError which arises
-- when they are closed.
flush_outerr = do
   flush stdout
   flush stderr
   where
      flush h = hFlush h `catch` (\ioe -> if isIllegalOperation ioe then return () else ioError ioe)


-- ProcessStatus doesn't derive Typeable.
{-
data ProcessStatus = Exited ExitCode
                   | Terminated Signal
                   | Stopped Signal
		   deriving (Eq, Ord, Show)
-}

-- For GHC-7.8:
deriving instance Typeable ProcessStatus

{- Pre-7.8-Stuff:
instance Typeable ProcessStatus where
   typeOf = const tyCon_ProcessStatus

-- GHC 6.4
tyCon_ProcessStatus = mkTyConApp (mkTyCon3 "hsshellscript"
                                           "HsShellScript.ProcErr"
                                           "Posix.ProcessStatus") []
-}


-- | The GHC libraries don't declare @Foreign.C.Error.Errno@ as instance of
-- Show. This makes it up.
instance Show Foreign.C.Error.Errno where
   show (Errno e) = show e




----------------------------------------------------------------------------------------------------
-- Transmission of at most one IOError through a pipe (as far as that's possible).
-- This is used by execute_file to send the IOError of a failed exec...-call to the parent process.
--
-- Can't be transmitted:
--   - the handle field (of course...)
--   - IOErrors of the type DynIOError. They carry a dynamic value, with no provisions for serialization.
--
-- See base.GHC.IO.lhs


-- Read a single possible IOError from a file descriptor. The stream must be
-- closed on the other side after writing either nothing or a single IOError to
-- it.
receive_ioerror :: Fd -> IO (Maybe IOError)
receive_ioerror fd = do
   h <- System.Posix.fdToHandle fd
   txt <- hGetContents h
   seq (length txt) (return ())
   hClose h
   return (decode_ioerror txt)


-- Write a single IOError to a file descriptor, and close it.
send_ioerror fd ioe = do
   h <- System.Posix.fdToHandle fd
   Foreign.C.Error.getErrno
   hPutStr h (encode_ioerror ioe)
   hClose h


encode_ioerror :: IOError -> String
encode_ioerror ioe =
   show (ioetype_num ioe, ioe_location ioe, ioe_description ioe, ioe_filename ioe, ioe_errno ioe)


decode_ioerror :: String -> Maybe IOError
decode_ioerror txt =
   case txt of
      "" -> Nothing
      _  -> let (type_nr, location, description, filename, errno) = read txt
            in (Just (IOError { ioe_handle      = Nothing,
                                ioe_type        = num_ioetype type_nr,
                                ioe_location    = location,
                                ioe_description = description,
                                ioe_filename    = filename,
                                ioe_errno       = errno
                              }))

-- All IOError types in GHC 6.4, taken from the source code of GHC.IO.
-- Used only for serializing IOErrors which are thrown by executeFile, so this should never go out of date.
ioe_types = [(AlreadyExists, 1), (NoSuchThing, 2), (ResourceBusy, 3), (ResourceExhausted, 4), (EOF, 5), (IllegalOperation, 6), (PermissionDenied, 7),
             (UserError, 8), (UnsatisfiedConstraints, 9), (SystemError, 10), (ProtocolError, 11), (OtherError, 12), (InvalidArgument, 13),
             (InappropriateType, 14), (HardwareFault, 15), (UnsupportedOperation, 16), (TimeExpired, 17), (ResourceVanished, 18), (Interrupted, 19)]

-- IOError type as a number
ioetype_num ioe =
   case ioeGetErrorType ioe of
        ioetype    -> case lookup ioetype ioe_types of
                         Just num -> num
                         Nothing  -> error "Bug in HsShellScript: Unknown IOError type, can't serialize it."

-- IOError type from the number
num_ioetype num =
   case lookup num (map (\(a,b) -> (b,a)) ioe_types) of
      Just ioetype -> ioetype
      Nothing      -> error ("Bug in HsShellScript: Unknown IOError type number " ++ show num)


instance Exception ProcessStatus


-- | Determine the terminal width in columns. 
--
-- This value can be used to format output to fit the terminal.
--
-- This queries the terminal which is connected to stdout. It may happen, that
-- stdout isn't connected to a terminal. For instance when the program is part of
-- a pipe. In this case, an IOError is thrown.
--
-- See 'terminal_width', 'make_usage_info', 'print_usage_info', 'usage_info', 'wrap'.
terminal_width_ioe :: Handle                -- ^ Handle, which is connected to the terminal    
                   -> IO Int                -- ^ The number of columns in the constrolling terminal. 
                                            --   Throws an IOError when the handle isn't connected to a terminal.
terminal_width_ioe h = do

   fd <- unsafeWithHandleFd h $ \fd -> do 
      res <- {#call c_terminal_width#} (fromIntegral fd)
      when (res == -1) $ throwErrno' "terminal_width" Nothing Nothing
      return res
   return (fromIntegral fd)


-- | Determine the terminal width in columns. 
--
-- This value can be used to format output to fit the terminal.
--
-- This queries the terminal which is connected to stdout. It may happen, that
-- stdout isn't connected to a terminal, for instance when the program is part of
-- a pipe. In this case, @Nothing@ is returned. No exception is thrown.
--
-- See 'terminal_width_ioe', 'make_usage_info', 'print_usage_info', 'usage_info', 'wrap'.
terminal_width :: Handle                -- ^ Handle, which is connected to the terminal    
               -> IO (Maybe Int)        -- ^ The number of columns in the constrolling terminal. 
                                    --   Nothing, when the handle isn't connected to a terminal.
terminal_width h = do
   w <- terminal_width_ioe h
   return (Just w)
   `catch` (\(ioe :: IOError) -> return Nothing)



#c
/*
c2hs-0.14.5 chokes on the following includes.
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include <stdio.h>
*/
char *strerror(int errnum);
int fork(void);
void _exit(int status);
int isatty(int desc);
int close(int fd);
int dup2(int oldfd, int newfd);



/* Save all file descriptor flags in an array */
int* c_save_fdflags(void);

/* Restore all file descriptor flags from the array, and free it */
void c_restore_fdflags(int* flags);

/* Duplicate a file descriptor, allocating the new one at min or above */
int c_fcntl_dupfd(int fd, int min);

/* Prepare all file descriptors for a subsequent exec */
void c_prepare_fd_flags_for_exec(void);

/* Set a file descriptor to "close on exec" mode. Returns the old flags. */
int c_close_on_exec(int fd);

/* Set the flags of a file descriptor. Returns the old flags. */
int c_set_flags(int fd, int new_flags);

/* Determine the with of the controlling terminal */
int c_terminal_width(int fd);
#endc
