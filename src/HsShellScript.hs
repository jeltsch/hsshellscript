-- HsShellScript main module
module HsShellScript (
              -- * Command Line Arguments Command line arguments are handled by the module "HsShellScript.Args",
              -- which is reexported by "HsShellScript".
              module HsShellScript.Args,

              -- * Paths and Directories
              mkdir, rmdir, pwd, cd, realpath, realpath_s, path_exists, path_exists', is_file, is_dir, with_wd,
              -- ** Parsing and Composing Paths
              module HsShellScript.Paths,

              -- * Symbolic Links
              is_symlink, symlink, readlink, readlink',

              -- * Manipulating Files
              rm, chmod, chown, cp, mv,
              HsShellScript.Commands.rename, rename_mv, force_rename, force_mv, force_rename_mv, force_cmd,
              force_writeable, force_writeable2,
              getFileStatus', fileAccess', setFileMode',

              -- * Interfaces to Some Specific External Commands
              mt_status, fdupes, du,

              -- * Calling External Programs

              -- ** Running a Subroutine in a Separate Process
              -- $subr

              -- ** About the @exec@ Functions
              -- $exec

              -- ** Functions for Forking Child Processes and Calling External Programs
              subproc,
              spawn,
              runprog, RunError(..), show_runerror, to_ioe, as_ioe,
              exec, execp, exece, execpe,
              echo, silently,
              system_runprog, system_throw, execute_file,
              child,
              explain_processstatus,
              call, run,

              -- * Redirecting Input and Output
              (->-), (->>-), (=>-), (=>>-), (-<-),
              (-&>-), (-&>>-),
              err_to_out, out_to_err,

              -- * Pipes

              -- ** File Descriptors in Pipes
              -- $fdpipes

              -- ** Pipe Creation Functions
              (-|-), (=|-), (-|=), (=|=),
              redirect,
              pipe_to, h_pipe_to,
              pipe_from, lazy_pipe_from, h_pipe_from,
              pipe_from2, lazy_pipe_from2, h_pipe_from2,
              pipe_from_full, pipe_from_full2, 
              pipes,

              -- * Shell-like Quoting
              module HsShellScript.Shell,

              -- * Creating temporary files and directories
              tmp_file, tmp_dir, temp_file, temp_dir, temp_path, with_tmp_file, with_tmp_dir, with_temp_file,
              with_temp_dir,

              -- * Reading mount information
              Mntent(..), read_mounts, read_mtab, read_fstab,

              -- * Output to the standard stream, colorful logging and error reporting
              outm, outm_, logm, logm_, errm, errm_,
              isatty,
              terminal_width, terminal_width_ioe,


              -- * Miscellaneous
              zeros, chomp, lazy_contents, contents, glob, glob_quote,

              -- * Error Handling
              mainwrapper, errno,
              strerror,
              perror',
              perror,
              _exit,
              HsShellScript.ProcErr.failIO,
              exitcode,
              throwErrno',
              show_ioerror,
              fill_in_filename, fill_in_location, add_location
         )
where

import Control.Exception
import GHC.IO
import HsShellScript.Args
import HsShellScript.Commands
import HsShellScript.Misc
import HsShellScript.Paths
import HsShellScript.ProcErr
import HsShellScript.Shell
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.Posix



{- | Error reporting wrapper for the @main@ function. This catches any
   HsShellScript generated exceptions, and @IOError@s, prints
   an error message and exits with @exitFailure@. The @main@ function
   typically looks like this:

   >main = mainwrapper $ do ...

   The exceptions caught are 'ArgError', 'RunError', 'ProcessStatus' and @IOError@.
-}
mainwrapper :: IO a     -- ^ Should be @main@
            -> IO a     -- ^ Wrapped @main@
mainwrapper io =
    io
    `catches` [ Handler $ \(argerror :: ArgError) ->
                   do errm (argerror_message argerror)
                      -- print_usage_info stdout "\nThe possible command line arguments are:\n\n" descs
                      exitFailure
              , Handler $ \(processstatus :: ProcessStatus) ->
                   do errm $ "Process error. Process status = " ++ show ( processstatus :: ProcessStatus )
                      exitFailure
              , Handler $ \(runerror :: RunError) ->
                   do errm (show_runerror runerror)
                      exitFailure
              , Handler $ \(ioe :: IOError) ->
                   do errm (show_ioerror ioe)
                      exitFailure
              ]

{- $fdpipes
   #fdpipes#

   With HsShellScript, you build pipes from IO actions, which can replace
   themselves with an external program via a variant of @exec@. It's mostly
   transparent whether some part of the pipe is a subroutine of the main
   program, or an external program.

   But actually, there are two cases. When the forked process is a subroutine,
   the child's @stdin@ handle is connected to the parent. On the other hand,
   when the forked process consists of calling an @exec@ variant, that program's
   file descriptor 0 is to be connected to the parent process.

   Normally, @stdin@ connects exactly to file descriptor 0, but this isn't
   necessarily the case. For instance, when @stdin@ has been closed, the file
   descriptor will be reused on the next occasion. When it is reopened again
   by calling @GHC.Handle.hDuplicateTo h stdin@, then the new @stdin@
   will be using a different file descriptor, and file descriptor 0 will be in
   use by another handle. Thus, when forking a subroutine, we're connected via
   @stdin@, but we can't expect to be connected via file descriptor 0.

   In case the child process is to be replaced with another program, we need to
   make sure that right file descriptor connects to the parent process. This is
   accomplished by the @exec@ functions. They replace the standard file
   descriptors with the ones that the standard handles currently use. See
   "HsShellScript#exec" for details.

   These two examples work as expected.

   Example 1:

>-- This closes stdin.
>c <- contents "-"
>
>pipe_to something
>   (     -- execp arranges for "something" to go to foo's file descriptor 0
>         execp "foo" []
>
>     -|- (do -- Read foo's standard output from new stdin handle
>             c' <- lazy_contents "-"
>             ...
>         )
>   )

   Example 2:

>-- Call wc to count the number of lines in txt
>count <- fmap (read . chomp) $
>              pipe_from (putStr txt -|= execp "wc" ["-l"])

-}


{- $subr
   #subr#

   It can by very useful to fork a child process, which executes a subroutine of
   the main program. In the following example, paths are piped to the @recode@
   program in order to convert them from ISO 8859-1 to UTF-8. Its output is read
   by a subroutine of the main program, which can use it to rename the files.

>main = mainwrapper $ do
>   paths <- contents "-"
>   pipe_to paths $
>           (     execp "recode" ["-f", "latin1..utf8"]
>             -|= (do paths_utf8 <- lazy_contents "-"
>                     mapM_ (\(path, path_utf8) ->
>                               ...
>                           )
>                           (zip (lines paths) (lines paths_utf8))
>                 )
>           )

   The same could be achieved this way:

>main = mainwrapper $ do
>   paths <- contents "-"
>   paths_utf8 <-
>      pipe_from (     putStr paths
>                  -|= execp "recode" ["-f", "latin1..utf8"]
>                )
>   mapM_ (\(path, path_utf8) ->
>             ...
>         )
>         (zip (lines paths) (lines paths_utf8))

   Most of the time, it's intuitive. But sometimes, the forked subroutine
   interferes with the parent process.

   When the process clones itself by calling @fork(2)@, everything gets
   duplicated - open files, database connections, window system connections...
   This becomes an issue when the child process uses any of it. For instance,
   any buffered, not yet written data associated with a file handle gets
   duplicated. When the child process uses that handle, that data gets written
   twice.

   The functions which fork a child process ('subproc', 'spawn', 'silently',
   'pipe_to' etc.) flush @stdout@ and @stderr@ (should be unbuffered) before the
   fork. So the child process can use them. The pipe functions also take care of
   @stdin@, which is used to read from the pipe. But they don't know about any
   other handles.

   What happens when the subroutine finishes? The control flow would escape into
   the main program, doing unexpected things. Therefore the functions which fork
   an IO action terminate the child process when the subroutine finishes. They
   do so by calling '_exit', circumventing normal program shutdown. Normal
   shutdown would flush cloned file handles, shut down database connections now
   shared with the parent process etc. Only the @stdout@ and @stderr@ are
   flushed before. If the child process requires any more cleanup on
   termination, such as flushing new file handles created in the child process,
   it's the responsibility of the programmer to do so before the subroutine
   exits.

   When the subroutine throws an exception, the control flow isn't allowed to
   escape into the main program either. Any exception is caught, an error
   message is printed, and the child process is terminated with @_exit 1@.

   The subroutine /must not/ terminate the child process normally, by calling
   @exitWith@ or @exitFailure@. It should terminate with '_exit'. Don't forget
   to flush @stdout@ before, which won't be line buffered when not connected to
   a terminal. It can also just leave the subroutine. The functions which fork
   child processes intercept any attempt of normal program shutdown in the child
   process (it's an @ExitException@, see the GHC library documentation). A
   warning message is printed, and the child is terminated with @_exit@, with
   the same exit code which it would have been.
-}


{- $exec
   #exec#

   There are five @exec@ variants: 'exec', 'execp', 'exece', 'execpe' and
   'execute_file'. The first four are frontends to @execute_file@. They
   differ in whether the @PATH@ is searched, and in whether a new environment is
   installed. The latter is a replacement for
   @System.Posix.Process.executeFile@. They are designed to work intuitively in
   conjunction with the functions which fork a child process, such as 'run',
   'call', 'spawn', 'pipe_to' etc.

   Before replacing the process, @stdout@ and @stderr@ are flushed, so no yet
   unwritten data is lost. Then the file descriptors of the process are prepared
   for the exec, such that everything works as expected. The standard file
   descriptors 0-2 are made to correspond to the standard handles again (this
   might have changed, see "HsShellScript#exec"). They are also reset to
   blocking mode. All others are closed when the exec succeeds.

   /You can't use/ @executeFile@ /directly, unless you take care of the things/
   /outlined at/ "HsShellScript#exec" /and/ 'execute_file' /by yourself./

   If replacing the process fails (for instance, because the program wasn't
   found), then everything is restored to original state, and an @IOError@ is
   thrown, and the process continues with normal error handling. Normally, the
   @exec@ functions are used in conjunction with some of the functions which
   fork a child process. They also handle errors, so the forked action doesn't
   need to deal with failure of @exec@. The error handling and
   termination is done via the 'child' function.

   Sometimes you want to pass an open file descriptor to the program. In this
   case, you can't use the @exec@ variants. You need to call @executeFile@
   directly, and take care of the outlined matters by yourself. In this
   case, take a look at the source code of @execute_file@.

   For full details, see the documentation of 'execute_file'.
-}
