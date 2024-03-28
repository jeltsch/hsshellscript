-- #hide
module HsShellScript.Commands where


import Prelude hiding (catch)
import Control.Exception
import Data.Bits
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import GHC.IO hiding (bracket)
import GHC.IO.Exception                 -- InvalidArgument, UnsupportedOperation
import HsShellScript.Misc
import HsShellScript.Misc
import HsShellScript.Paths
import HsShellScript.ProcErr
import HsShellScript.Shell
import System.IO.Error hiding (catch)
import Data.List
import Data.Maybe
import Control.Monad
import Control.Exception
import Text.ParserCombinators.Parsec as Parsec
import System.Posix hiding (rename, createDirectory, removeDirectory)
import System.Random
import System.Directory

-- | Do a call to the @realpath(3)@ system library function. This makes the path absolute, normalizes it and
-- expands all symbolic links. In case of an error, an @IOError@ is thrown.
realpath :: String    -- ^ path
         -> IO String -- ^ noramlized, absolute path, with symbolic links expanded
realpath path =
   withCString path $ \cpath -> do
      res <- {#call hsshellscript_get_realpath#} cpath
      if res == nullPtr
         then throwErrno' "realpath" Nothing (Just path)
         else peekCString res

-- | Determine the target of a symbolic link. This uses the @readlink(2)@ system call. The result is a path which
-- is either absolute, or relative to the directory which the symlink is in. In case of an error, an @IOError@ is
-- thrown. The path is included and can be accessed with @IO.ioeGetFileName@. Note that, if the path to the symlink
-- ends with a slash, this path denotes the directory pointed to, /not/ the symlink. In this case the call to will
-- fail because of \"Invalid argument\".
readlink :: String    -- ^ Path of the symbolic link
         -> IO String -- ^ The link target - where the symbolic link points to
readlink path =
   withCString path $ \cpath -> do
      res <- {#call hsshellscript_get_readlink#} cpath
      if res == nullPtr
         then throwErrno' "readlink" Nothing (Just path)
         else peekCString res

-- | Determine the target of a symbolic link. This uses the @readlink(2)@ system call. The target is converted,
-- such that it is relative to the current working directory, if it isn't absolute. Note that, if the path to the
-- symlink ends with a slash, this path denotes the directory pointed to, /not/ the symlink. In this case the call
-- to @readlink@ will fail with an @IOError@ because of \"Invalid argument\". In case of any error, a proper
-- @IOError@ is thrown.
readlink' :: String     -- ^ path of the symbolic link
          -> IO String  -- ^ target; where the symbolic link points to
readlink' symlink = do
   target <- readlink symlink
   return (absolute_path' target (fst (split_path symlink)))


-- | Determine whether a path is a symbolic link. The result for a dangling symlink is @True@. The path must exist
-- in the file system. In case of an error, a proper @IOError@ is thrown.
is_symlink :: String    -- ^ path
           -> IO Bool   -- ^ Whether the path is a symbolic link.
is_symlink path =
    do fill_in_location "is_symlink" $ readlink path
       return True
    `catch`
       (\(ioe::IOError) -> if (ioeGetErrorType ioe == InvalidArgument) then return False else ioError ioe)


-- | Return the normalised, absolute version of a specified path. The path is made absolute with the current
-- working directory, and is syntactically normalised afterwards. This is the same as what the @realpath@ program
-- reports with the @-s@ option. It's almost the same as what it reports when called from a shell. The difference
-- lies in the shell's idea of the current working directory. See 'cd' for details.
--
-- See 'cd', 'normalise_path'.
realpath_s :: String    -- ^ path
           -> IO String -- ^ noramlized, absolute path, with symbolic links not expanded
realpath_s pfad =
   do cwd <- getCurrentDirectory
      return (normalise_path (absolute_path_by cwd pfad))


-- | Make a symbolic link. This is the @symlink(2)@ function. Any error results in an @IOError@ thrown. The path of
-- the intended symlink is included in the @IOError@ and can be accessed with @ioeGetFileName@ from the Haskell
-- standard library @IO@.
symlink :: String       -- ^ contents of the symlink
        -> String       -- ^ path of the symlink
        -> IO ()
symlink oldpath newpath = do
   o <- newCString oldpath
   n <- newCString newpath
   res <- {#call symlink as foreign_symlink#} o n
   when (res == -1) $ throwErrno' ("symlink " ++ shell_quote oldpath ++ " to " ++ shell_quote newpath) Nothing (Just newpath)


-- | Call the @du@ program. See du(1).
du :: (Integral int, Read int, Show int)
   => int               -- ^ block size, this is the @--block-size@ option.
   -> String            -- ^ path of the file or directory to determine the size of
   -> IO int            -- ^ size in blocks
du block_gr pfad =
    let par = ["--summarize", "--block-size=" ++ show block_gr, pfad]
        parsen ausg =
           case reads ausg of
              [(groesse, _)] -> return groesse
              _              -> errm ("Can't parse the output of the \"du\" program: \n" ++ quote ausg ++ "\nShell command: " ++ shell_command "du" par)
                                >> fail ("Parse error: " ++ ausg)
    in pipe_from (exec "/usr/bin/du" par) >>= parsen



-- | Create directory. This is a shorthand to @System.Directory.createDirectory@ from the Haskell standard library.
-- In case of an error, the path is included in the @IOError@, which GHC's implementation neglects to do.
mkdir :: String         -- ^ path
      -> IO ()
mkdir path = 
   createDirectory path 
   `catch` (\(ioe::IOError) -> ioError (ioe { ioe_filename = Just path }))


-- | Remove directory. This is @Directory.removeDirectory@ from the Haskell standard library. In case of an error,
-- the path is included in the @IOError@, which GHC's implementation neglects to do.
rmdir :: String         -- ^ path
      -> IO ()
rmdir path = 
   removeDirectory path 
   `catch` (\(ioe::IOError) -> ioError (ioe { ioe_filename = Just path }))


-- | Remove file. This is @Directory.removeFile@ from the Haskell standard library, which is a direct frontend to
-- the @unlink(2)@ system call in GHC.
rm :: String         -- ^ path
   -> IO ()
rm = removeFile


{- | Change directory. This is an alias for @Directory.setCurrentDirectory@ from the Haskell standard library. In
case of an error, the path is included in the @IOError@, which GHC's implementation neglects to do.

Note that this command is subtly different from the shell's @cd@ command. It changes the process' working
directory. This is always a realpath. Symlinks are expanded. The shell, on the other hand, keeps track of the
current working directory separately, in a different way: symlinks are /not/ expanded. The shell's idea of the
working directory is different from the working directory which a process has.

This means that the same sequence of @cd@ commands, when done in a real shell script, will lead into the same
directory. But the working directory as reported by the shell's @pwd@ command may differ from the corresponding
one, reported by @getCurrentDirectory@.

(When talking about the \"shell\", I'm talking about bash, regardless of whether started as @\/bin\/bash@ or in
compatibility mode, as @\/bin\/sh@. I presume it's the standard behavior for the POSIX standard shell.)

See 'pwd', 'with_wd'
-}
cd :: String         -- ^ path
   -> IO ()
cd path = 
   setCurrentDirectory path 
   `catch` (\(ioe::IOError) -> ioError (ioe { ioe_filename = Just path }))


-- |
-- Get program start working directory. This is the @PWD@ environent
-- variable, which is kept by the shell (bash, at least). It records the
-- directory path in which the program has been started. Symbolic links in
-- this path aren't expanded. In this way, it differs from
-- @getCurrentDirectory@ from the Haskell standard library.
--   
-- See 'cd', 'with_wd'
pwd :: IO String
pwd = fmap (fromMaybe "") (System.Posix.getEnv "PWD")



-- | Change the working directory temporarily. This executes the specified IO action with a new working directory,
-- and restores it afterwards (exception-safely).
with_wd :: FilePath     -- ^ New working directory
        -> IO a         -- ^ Action to run
        -> IO a
with_wd wd io =
   bracket (do cwd <- getCurrentDirectory
               setCurrentDirectory wd
               return cwd)
           (\cwd -> setCurrentDirectory cwd)
           (const io)



{- | Execute @\/bin\/chmod@

>chmod = run "/bin/chmod"
-}
chmod :: [String]       -- ^ Command line arguments
      -> IO ()
chmod = run "/bin/chmod"


{- | Execute @\/bin\/chown@

>chown = run "/bin/chown"
-}
chown :: [String]       -- ^ Command line arguments
      -> IO ()
chown = run "/bin/chown"


-- |
-- Execute the cp program
cp :: String    -- ^ source
   -> String    -- ^ destination
   -> IO ()
cp from to =
   run "cp" [from, to]


-- |
-- Execute the mv program. 
--
-- This calls the @\/bin\/mv@ to rename a file, or move it to another directory. You can move a file to another
-- file system with this. This starts a new process, which is rather slow. Consider using @rename@ instead, when
-- possible.
--
-- See 'rename'.
mv :: String    -- ^ source
   -> String    -- ^ destination
   -> IO ()
mv from to = runprog "/bin/mv" ["--", from, to]


number  :: Parser Int
number  = do sgn <- ( (char '-' >> return (-1))
                      <|> return 1
                    )
             ds <- many1 digit
             return (sgn * read ds)
          <?> "number"

-- Parser for the output of the "mt status" command.
parse_mt_status :: Parser ( Int    -- file number
                          , Int    -- block number
                          )
parse_mt_status =
   do (fn,bn) <- parse_mt_status' (Nothing, Nothing)
      return (fromJust fn, fromJust bn)
   where
      try = Parsec.try

      parse_mt_status' :: (Maybe Int, Maybe Int) -> Parser (Maybe Int, Maybe Int)
      parse_mt_status' st = do
         st' <- parse_mt_status1' st
         ( parse_mt_status' st' <|> return st' )

      parse_mt_status1' :: (Maybe Int, Maybe Int) -> Parser (Maybe Int, Maybe Int)
      parse_mt_status1' st@(fn,bn) =
             try (do string "file number = "
                     nr <- number
                     newline
                     return (Just nr, bn)
                 )
         <|> try (do string "block number = "
                     nr <- number
                     newline
                     return (fn, Just nr)
                 )
         <|> (manyTill anyChar newline >> return st)

-- |
-- Run the command @mt status@ for querying the tape drive status, and
-- parse its output.
mt_status :: IO (Int, Int)      -- ^ file and block number
mt_status = do
   out <- pipe_from (exec "/bin/mt" ["status"])
   case (parse parse_mt_status "" out) of
      Left err -> ioError (userError ("parse error at " ++ show err))
      Right x  -> return x



-- | The @rename(2)@ system call to rename and\/or move a file. The @renameFile@ action from the Haskell standard
-- library doesn\'t do it, because the two paths may not refer to directories. Failure results in an @IOError@
-- thrown. The /new/ path is included in the @IOError@ and can be accessed with @IO.ioeGetFileName@.
rename :: String        -- ^ Old path
       -> String        -- ^ New path
       -> IO ()
rename oldpath newpath = do
   withCString oldpath $ \coldpath ->
      withCString newpath $ \cnewpath -> do
         res <- {#call rename as foreign_rename#} coldpath cnewpath
         when (res == -1) $ throwErrno' ("rename " ++ shell_quote oldpath ++ " to " ++ shell_quote newpath) Nothing (Just newpath)



-- | Rename a file. This first tries 'rename', which is most efficient. If it fails, because source and target path
-- point to different file systems (as indicated by the @errno@ value @EXDEV@), then @\/bin\/mv@ is called.
--
-- See 'rename', 'mv'.
rename_mv :: FilePath           -- ^ Old path
          -> FilePath           -- ^ New path
          -> IO ()
rename_mv old new =
   HsShellScript.Commands.rename old new
      `catch` (\(ioe::IOError) -> 
                          if ioeGetErrorType ioe == UnsupportedOperation
                             then do errno <- getErrno
                                     -- Foreign.C.Error.errnoToIOError matches many errno values to
                                     -- UnsupportedOperation. In order to determine if it is the right one, the
                                     -- errno is taken again. This relies on no system calls in between.
                                     if (errno == eXDEV)
                                        then run "/bin/mv" ["--", old, new]
                                        else ioError ioe
                             else ioError ioe
                 )


{- | Rename a file or directory, and manage read only issues.

This renames a file or directory, using @rename@, sets the necessary write permissions beforehand, and restores
them afterwards. This is more efficient than @force_mv@, because no external program needs to be called, but it can
rename files only inside the same file system. See @force_cmd@ for a detailed description.

The new path may be an existing directory. In this case, it is assumed that the old file is to be moved into this
directory (like with @mv@). The new path is then completed with the file name component of the old path. You won't
get an \"already exists\" error.

>force_rename = force_cmd rename

See 'force_cmd', 'rename'.
-}
force_rename :: String        -- ^ Old path
             -> String        -- ^ New path
             -> IO ()
force_rename = force_cmd HsShellScript.Commands.rename


{- | Move a file or directory, and manage read only issues.

This moves a file or directory, using the external command @mv@, sets the necessary write permissions beforehand,
and restores them afterwards. This is less efficient than @force_rename@, because the external program @mv@ needs
to be called, but it can move files between file systems. See @force_cmd@ for a detailed description.

>force_mv src tgt = fill_in_location "force_mv" $ force_cmd (\src tgt -> run "/bin/mv" ["--", src, tgt]) src tgt

See 'force_cmd', 'force_mv'.
-}
force_mv :: String        -- ^ Old path
         -> String        -- ^ New path or target directory
         -> IO ()
force_mv src tgt =
   fill_in_location "force_mv" $
      force_cmd (\src tgt -> run "/bin/mv" ["--", src, tgt]) src tgt


{- | Rename a file with 'rename', or when necessary with 'mv', and manage read only issues.

The necessary write permissions are set, then the file is renamed, then the permissions are restored.

First, the 'rename' system call is tried, which is most efficient. If it fails, because source and target path
point to different file systems (as indicated by the @errno@ value @EXDEV@), then @\/bin\/mv@ is called.

>force_rename_mv old new = fill_in_location "force_rename_mv" $ force_cmd rename_mv old new

See 'rename_mv', 'rename', 'mv', 'force_cmd'.
-}
force_rename_mv :: FilePath           -- ^ Old path
                -> FilePath           -- ^ New path
                -> IO ()
force_rename_mv old new =
   fill_in_location "force_rename_mv" $
      force_cmd rename_mv old new


{- | Call a command which moves a file or directory, and manage read only issues.

This function is for calling a command, which renames files. Beforehand, write permissions are set in order to
enable the operation, and afterwards the permissions are restored. The command is meant to be something like
@rename@ or @run \"\/bin\/mv\"@.

In order to change the name of a file or dirctory, but leave it in the super directory it is in, the super
directory must be writeable. In order to move a file or directory to a different super directory, both super
directories and the file\/directory to be moved must be writeable. I don't know what this behaviour is supposed to
be good for.

This function concerns itself with the case that the file\/directory to be moved or renamed, or the super
directories are read only. It makes the necessary places writeable, calls the command, and makes them read only
again, if they were before. The user needs the necessary permissions for changing the corresponding write
permissions. If an error occurs (such as file not found, or insufficient permissions), then the write permissions
are restored to the state before, before the exception is passed through to the caller.

The command must take two arguments, the old path and the new path. It is expected to create the new path in the
file system, such that the correct write permissions of the new path can be set by @force_cmd@ after executing it.

The new path may be an existing directory. In this case, it is assumed that the old file is to be moved into this
directory (like with @mv@). The new path is completed with the file name component of the old path, before it is
passed to the command, such that the command is supplied the complete new path.

Examples:

>force_cmd rename from to
>force_cmd (\from to -> run "/bin/mv" ["-i", "-v", "--", from, to]) from to

See 'force_rename', 'force_mv', 'rename'.
-}
force_cmd :: (String -> String -> IO ())        -- ^ Command to execute after preparing the permissions
          -> String                             -- ^ Old path
          -> String                             -- ^ New path or target directory
          -> IO ()
force_cmd cmd oldpath newpath0 =
   do isdir <- is_dir newpath0
      let newpath = if isdir then newpath0 ++ "/" ++ snd (split_path oldpath) else newpath0

      old_abs <- absolute_path oldpath
      new_abs <- absolute_path newpath
      let (olddir, _) = split_path old_abs
          (newdir, _) = split_path new_abs
      if olddir == newdir
         then -- Don't need to make the file/directory writeable.
              force_writeable olddir (cmd oldpath newpath)
         else -- Need to make both the file/dirctory and both super directories writeable.
              let cmd' = do res <- cmd oldpath newpath
                            return (newpath, res)
              in  force_writeable olddir (force_writeable newdir (force_writeable2 oldpath cmd'))
   `catch`
      (\(ioe::IOError) -> 
          ioError (if ioe_location ioe == "" || ioe_location ioe == "force_writeable" 
                      then ioe { ioe_location = "force_cmd" } 
                      else ioe))



{- | Make a file or directory writeable for the user, perform an action, and restore its writeable status. An
IOError is raised when the user doesn't have permission to make the file or directory writeable.

>force_writeable path io = force_writeable2 path (io >>= \res -> return (path, res))

Example:

>-- Need to create a new directory in /foo/bar, even if that's write protected
>force_writeable "/foo/bar" $ mkdir "/foo/bar/baz"

See 'force_cmd', 'force_writeable2'.
-}
force_writeable :: String    -- ^ File or directory to make writeable
                -> IO a      -- ^ Action to perform
                -> IO a      -- ^ Returns the return value of the action
force_writeable path io =
   add_location "force_writeable" $
      force_writeable2 path (io >>= \res -> return (path, res))


{- | Make a file or directory writeable for the user, perform an action, and restore its writeable status. The
action may change the name of the file or directory. Therefore it returns the new name, along with another return
value, which is passed to the caller.

The writeable status is only changed back if it has been changed by @force_writeable2@ before. An IOError is
raised when the user doesn'h have permission to make the file or directory writeable, or when the new path
doesn't exist.

See 'force_cmd', 'force_writeable'.
-}
force_writeable2 :: String          -- ^ File or directory to make writeable
                 -> IO (String, a)  -- ^ Action to perform
                 -> IO a
force_writeable2 path_before io =
   add_location "force_writeable2" $
      do writeable <- fileAccess' path_before False True False
         when (not writeable) $ set_user_writeable path_before
         (path_after, res) <-
            catch
               io
               (\(e::SomeException) -> 
                      do when (not writeable) $
                            catch (set_user_readonly path_before)
                                  ignore                        -- Don't let failure to restore the status make
                                                                -- us loose the actual exception.
                         throwIO e
               )
         when (not writeable) $ set_user_readonly path_after
         return res

   where
      ignore :: SomeException -> IO ()
      ignore _ = return ()

      set_user_writeable path = do
         filemode <- fmap fileMode (getFileStatus' path)
         fill_in_filename path $ setFileMode' path (filemode .|. ownerWriteMode)

      set_user_readonly path = do
         filemode <- fmap fileMode (getFileStatus' path)
         fill_in_filename path $ setFileMode' path (filemode .&. (complement ownerWriteMode))


-- | Call the @fdupes@ program in order to find identical files. It outputs a list of groups of file names, such
-- that the files in each group are identical. Each of these groups is further analysed by the @fdupes@ action.
-- It is split to a list of lists of paths, such that each list of paths corresponds to one of the directories
-- which have been searched by the @fdupes@ program. If you just want groups of identical files, then apply @map
-- concat@ to the result.
--
-- /The/ @fdupes@ /program doesn\'t handle multiple occurences of the same directory, or in recursive mode one
-- specified directory containing another, properly. The same file may get reported multiple times, and identical
-- files may not get reported./
--
-- The paths are normalised (using 'normalise_path').
fdupes :: [String]              -- ^ Options for the fdupes program
       -> [String]              -- ^ Directories with files to compare
       -> IO [[[String]]]       -- ^ For each set of identical files, and each of the specified directories,
                                -- the paths of the identical files in this directory.
fdupes opts paths = do
   let paths'  = map normalise_path paths
       paths'' = map (++"/") paths'
   out <- fmap lines $ pipe_from (run "/usr/bin/fdupes" (opts ++ ["--"] ++ paths'))
   let grps = groups out
   return (map (sortgrp paths'') grps)
   where
      groups [] = []
      groups l =
         let l' = dropWhile (== "") l
             (g,rest) = span (/= "") l'
         in if g == [] then [] else (g : groups rest)

      split p [] = ([], [])
      split p (x:xs) =
         let (yes, no) = split p xs
         in  if p x then (x:yes, no)
                    else (yes, x:no)

      -- result: ( <paths within the directory>, <rest of paths> )
      path1 grp dir = split (isPrefixOf dir) grp

      -- super directories -> Group of identical files -> list of lists of files in each directory
      sortgrp dirs [] = map (const []) dirs
      sortgrp [] grp = error ("Bug: found paths which don't belong to any of the directories:\n" ++ show grp)
      sortgrp (dir:dirs) grp = let (paths1, grp_rest) = path1 grp dir
                               in  (paths1 : sortgrp dirs grp_rest)


replace_location :: String
                 -> String
                 -> IO a
                 -> IO a
replace_location was wodurch io =
   catch io
         (\(ioe::IOError) -> 
                  if ioe_location ioe == was
                     then ioError (ioe { ioe_location = wodurch })
                     else ioError ioe
         )


#c
/*
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <unistd.h>
#include <stdio.h>
*/
int symlink(const char *oldpath, const char *newpath);
int rename(const char *oldpath, const char *newpath);

char* hsshellscript_get_realpath(char* path);
char* hsshellscript_get_readlink(char* path);
#endc



