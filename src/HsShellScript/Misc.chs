-- #hide
module HsShellScript.Misc where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Typeable
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.C.String
import Foreign.Ptr
import GHC.IO hiding (finally, bracket)
import GHC.IO.Exception
import HsShellScript.ProcErr
import Prelude hiding (catch)
import System.Directory
import System.IO
import System.IO.Error hiding (catch)
import System.Posix hiding (removeDirectory)
import System.Random



-- | Format an @Int@ with leading zeros. If the string representation of the @Inŧ@ is longer than the number of
-- characters to fill up, this produces as many characters as needed.
zeros :: Int            -- ^ How many characters to fill up
      -> Int            -- ^ Value to represent as a string
      -> String         -- ^ String representation of the value, using the specified number of characters
zeros stellen z =
   let txt  = show z
       auff = stellen - length txt
       n    = take (if auff >= 0 then auff else 0) (repeat '0')
   in  n ++ txt


-- |
-- Remove trailing newlines. This is silimar to perl's @chomp@ procedure.
chomp :: String         -- ^ String to be chomped
      -> String         -- ^ Same string, except for no newline characters at the end
chomp "" = ""
chomp "\n" = ""
chomp [x] = [x]
chomp (x:xs) = let xs' = chomp xs
               in  if xs' == "" && x == '\n' then "" else x:xs'


{- | Get contents of a file or of @stdin@. This is a simple frontend to @hGetContents@. A file name of @\"-\"@
designates stdin. The contents are read lazily as the string is evaluated.

(The handle which we read from will be in semi-closed state. Once all input has read, it is closed automatically
(Haskell Library Report 11.2.1). Therefore we don't need to return it).

>lazy_contents path = do
>    h   <- if path == "-" then return stdin else openFile path ReadMode
>    hGetContents h
-}
lazy_contents :: String                 -- ^ Either the name of a file, or @\"-\"@
              -> IO String              -- ^ The lazily read contents of the file or @stdin@.
lazy_contents path = do
    h <- if path == "-" then return stdin else openFile path ReadMode
    hGetContents h


-- | Get contents of a file or of @stdin@ eagerly. This is the same as @lazy_contents@, except for the contents
-- being read immediately.

contents :: String              -- ^ either the name of a file, or @\"-\"@ for @stdin@
         -> IO String           -- ^ the contents of the file or of standard input
contents pfad = do
    txt <- lazy_contents pfad
    seq (length txt) (return ())
    return txt


-- | Test for the existence of a path. This is the disjunction of @Directory.doesDirectoryExist@ and
-- @Directory.doesFileExist@. For an dangling symlink, this will return @False@.
path_exists :: String    -- ^ Path
            -> IO Bool   -- ^ Whether the path exists in the file system
path_exists pfad = do
    de <- doesDirectoryExist pfad
    fe <- doesFileExist pfad
    return (de || fe)


-- | Test for the existence of a path. This uses @System.Posix.Files.getFileStatus@ to determine whether the path
-- exists in any form in the file system. For a dangling symlink, the result is @True@.
path_exists' :: String    -- ^ Path
             -> IO Bool   -- ^ Whether the path exists in the file system
path_exists' path =
   catch (do getSymbolicLinkStatus path
             return True)
         (\(ioe :: IOError) -> 
             if isDoesNotExistError ioe then return False
                                        else ioError ioe)
             

-- | Test if path points to a directory. This will return @True@ for a symlink pointing to a directory. It's a
-- shortcut for @Directory.doesDirectoryExist@.
is_dir :: String        -- ^ Path
       -> IO Bool       -- ^ Whether the path exists and points to a directory.
is_dir = doesDirectoryExist


-- |
-- Test if path points to a file. This is a shortcut for
-- @Directory.doesFileExist@.
is_file :: String       -- ^ Path
        -> IO Bool      -- ^ Whether the path exists and points to a file.
is_file = doesFileExist


-- | This is the @System.Posix.Files.getFileStatus@ function from the GHC libraries, with improved error reporting.
-- The GHC function doesn't include the file name in the @IOError@ when the call fails, making error messages much
-- less useful. @getFileStatus\'@ rectifies this.
--
-- See 'System.Posix.Files.getFileStatus'.
getFileStatus' :: FilePath              -- ^ Path of the file, whose status is to be queried
               -> IO FileStatus         -- ^ Status of the file
getFileStatus' path =
   getFileStatus path
      `catch` (\ioe -> ioError (ioe { ioe_filename = Just path }))


-- | This is the @System.Posix.Files.fileAccess@ function from the GHC libraries, with improved error reporting.
-- The GHC function doesn't include the file name in the @IOError@ when the call fails, making error messages much
-- less useful. @fileAccess\'@ rectifies this.
--
-- See 'System.Posix.Files.fileAccess'.
fileAccess' :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess' p b c d =
   fileAccess p b c d
      `catch` (\ioe -> ioError (ioe { ioe_filename = Just p }))


-- | Create a temporary file. This will create a new, empty file, with a path which did not previously exist in the
-- file system. The path consists of the specified prefix, a sequence of random characters (digits and letters),
-- and the specified suffix. The file is created with read-write permissions for the user, and no permissons for
-- the group and others. The ownership is set to the effective user ID of the process. The group ownership is set
-- either to the effective group ID of the process or to the group ID of the parent directory (depending on
-- filesystem type and mount options on Linux - see @open(2)@ for details).
--
-- See 'tmp_file', 'temp_dir', 'with_temp_file'.
temp_file :: Int                        -- ^ Number of random characters to intersperse. Must be large enough,
                                        -- such that most combinations can't already
                                        -- exist.
          -> String                     -- ^ Prefix for the path to generate.
          -> String                     -- ^ Suffix for the path to generate.
          -> IO FilePath                -- ^ Path of the created file.
temp_file nr prefix suffix = do
   (fd, path) <- untilIO (do path <- temp_path nr prefix suffix
                             fd <- withCString path $ \cpath ->
                                {#call hsshellscript_open_nonvariadic#} cpath (o_CREAT .|. o_EXCL) 0o600
                             return (fd, path)
                         )
                         (\(fd, path) ->
                             if fd == -1 then do errno <- getErrno
                                                 when (errno /= eEXIST) $
                                                    throwErrno' "temp_file" Nothing (Just path)
                                                 return False
                                         else return True
                         )
   res <- {# call close as c_close #} fd
   when (res == -1) $ throwErrno' "temp_file" Nothing (Just path)
   return path

-- | Create a temporary directory. This will create a new directory, with a path which did not previously exist in
-- the file system. The path consists of the specified prefix, a sequence of random characters (digits and
-- letters), and the specified suffix. The directory is normally created with read-write-execute permissions for
-- the user, and no permissons for the group and others. But this may be further restricted by the process's umask
-- in the usual way.
--
-- The newly created directory will be owned by the effective uid of the process. If the directory containing the
-- it has the set group id bit set, or if the filesystem is mounted with BSD group semantics, the new directory
-- will inherit the group ownership from its parent; otherwise it will be owned by the effective gid of the
-- process. (See @mkdir(2)@)
--
-- See 'tmp_dir', 'temp_file', 'with_temp_dir'.
temp_dir :: Int                        -- ^ Number of random characters to intersperse. Must be large enough,
                                       -- such that most combinations can't already exist.
         -> String                     -- ^ Prefix for the path to generate.
         -> String                     -- ^ Suffix for the path to generate.
         -> IO FilePath                -- ^ Generated path.
temp_dir nr prefix suffix = do
   (_, path) <- untilIO (do path <- temp_path nr prefix suffix
                            ret <- withCString path $ \cpath -> {#call mkdir as c_mkdir#} cpath 0o700
                            return (ret, path)
                        )
                        (\(ret, path) ->
                            if ret == -1 then do errno <- getErrno
                                                 when (errno /= eEXIST) $
                                                    throwErrno' "temp_dir" Nothing (Just path)
                                                 return False
                                         else return True
                        )
   return path

-- | Create a temporary file. This will create a new, empty file, with read-write permissions for the user, and no
-- permissons for the group and others. The path consists of the specified prefix, a dot, and six random characters
-- (digits and letters).
--
-- @tmp_file prefix = temp_file 6 (prefix ++ \".\") \"\"@
--
-- See 'temp_file', 'tmp_dir', 'with_tmp_file'.
tmp_file :: String                     -- ^ Prefix for the path to generate.
         -> IO FilePath                -- ^ Path of the created file.
tmp_file prefix = temp_file 6 (prefix ++ ".") ""


-- | Create a temporary directory. This will create a new directory, with read-write-execute permissions for the
-- user (unless further restricted by the process's umask), and no permissons for the group and others. The path
-- consists of the specified prefix, a dot, and six random characters (digits and letters).
--
-- @tmp_dir prefix = temp_dir 6 (prefix ++ \".\") \"\"@
--
-- See 'temp_dir', 'tmp_file', 'with_tmp_dir'.
tmp_dir :: String                     -- ^ Prefix for the path to generate.
        -> IO FilePath                -- ^ Path of the created directory.
tmp_dir prefix = temp_dir 6 (prefix ++ ".") ""


-- | Create and open a temporary file, perform some action with it, and delete it afterwards. This is a front end
-- to the 'temp_file' function. The file and its path are created in the same way. The IO action is passed a handle
-- of the new file. When it finishes - normally or with an exception - the file is deleted.
--
-- See 'temp_file', 'with_tmp_file', 'with_temp_dir'.
with_temp_file :: Int                        -- ^ Number of random characters to intersperse. Must be large enough,
                                             -- such that most combinations can't
                                             -- already exist.
               -> String                     -- ^ Prefix for the path to generate.
               -> String                     -- ^ Suffix for the path to generate.
               -> (Handle -> IO a)           -- ^ Action to perform.
               -> IO a                       -- ^ Returns the value returned by the action.
with_temp_file nr prefix suffix io =
   bracket (do path <- temp_file nr prefix suffix
               h <- openFile path ReadWriteMode
               return (path, h)
           )
           (\(path,h) -> do
               hClose h
               removeFile path
           )
           (\(path,h) ->
               io h
           )



-- | Create a temporary directory, perform some action with it, and delete it afterwards. This is a front end to
-- the 'temp_dir' function. The directory and its path are created in the same way. The IO action is passed the
-- path of the new directory. When it finishes - normally or with an exception - the directory is deleted.
--
-- The action must clean up any files it creates inside the directory by itself. @with_temp_dir@ doesn't delete any
-- files inside, so the directory could be removed. If the directory isn't empty, an @IOError@ results (with the
-- path filled in). When the action throws an exception, and the temporary directory cannot be removed, then the
-- exception is passed through, rather than replacing it with the IOError. (This is because it's probably exactly
-- because of that exception that the directory isn't empty and can't be removed).
--
-- See 'temp_dir', 'with_tmp_dir', 'with_temp_file'.
with_temp_dir :: Int                        -- ^ Number of random characters to intersperse. Must be large enough,
                                            --   such that most combinations can't already exist.
              -> String                     -- ^ Prefix for the path to generate.
              -> String                     -- ^ Suffix for the path to generate.
              -> (FilePath -> IO a)         -- ^ Action to perform.
              -> IO a                       -- ^ Returns the value returned by the action.
with_temp_dir nr prefix suffix io = 
   do  path <- temp_dir nr prefix suffix
       a <- catch (io path)
                  (\e -> do remove path `catch` (\(e::SomeException) -> return ())
                            throw (e :: SomeException)
                  )
       remove path
       return a
   where
      remove path = removeDirectory path
                    `catch` (\ioe -> ioError (ioe { ioe_filename = Just path }))


-- | Create and open a temporary file, perform some action with it, and delete it afterwards. This is a front end
-- to the 'tmp_file' function. The file and its path are created in the same way. The IO action is passed a handle
-- of the new file. When it finishes - normally or with an exception - the file is deleted.
--
-- See 'tmp_file', 'with_temp_file', 'with_tmp_dir'.
with_tmp_file :: String                     -- ^ Prefix for the path to generate.
              -> (Handle -> IO a)           -- ^ Action to perform.
              -> IO a                       -- ^ Returns the value returned by the action.
with_tmp_file prefix io =
   bracket (do path <- tmp_file prefix
               h <- openFile path ReadWriteMode
               return (path, h)
           )
           (\(path,h) -> do
               hClose h
               removeFile path
           )
           (\(path,h) -> do
               e <- io h
               return e
          )

-- | Create a temporary directory, perform some action with it, and delete it afterwards. This is a front end to
-- the 'tmp_dir' function. The directory and its path are created in the same way. The IO action is passed the path
-- of the new directory. When it finishes - normally or with an exception - the directory is deleted.
--
-- The action must clean up any files it creates inside the directory by itself. @with_temp_dir@ doesn't delete any
-- files inside, so the directory could be removed. If the directory isn't empty, an @IOError@ results (with the
-- path filled in). When the action throws an exception, and the temporary directory cannot be removed, then the
-- exception is passed through, rather than replacing it with the IOError. (This is because it's probably exactly
-- because of that exception that the directory isn't empty and can't be removed).
--
-- >with_tmp_dir prefix io = with_temp_dir 6 (prefix ++ ".") "" io
--
-- See 'tmp_dir', 'with_temp_dir', 'with_tmp_file'.
with_tmp_dir :: String                     -- ^ Prefix for the path to generate.
             -> (FilePath -> IO a)         -- ^ Action to perform.
             -> IO a                       -- ^ Returns the value returned by the action.
with_tmp_dir prefix io = with_temp_dir 6 (prefix ++ ".") "" io


-- | Create a temporary path. This will generate a path which does not yet exist in the file system. It consists of
-- the specified prefix, a sequence of random characters (digits and letters), and the specified suffix.
--
-- /Avoid relying on the generated path not to exist in the file system./ Or else you'll get a potential race
-- condition, since some other process might create the path after @temp_path@, before you use it. This is a
-- security risk. The global random number generator (@Random.randomRIO@) is used to generate the random
-- characters. These might not be that random after all, and could potentially be guessed. Rather use @temp_file@
-- or @temp_dir@.
--
-- See 'temp_file', 'temp_dir'.
temp_path :: Int                        -- ^ Number of random characters to intersperse. Must be large enough,
                                        -- such that most combinations can't already exist.
          -> String                     -- ^ Prefix for the path to generate.
          -> String                     -- ^ Suffix for the path to generate.
          -> IO FilePath                -- ^ Generated path.
temp_path nr prefix suffix = do
   untilIO (do rand <- sequence (take nr (repeat (fmap char (randomRIO (0, 10+2*26 - 1)))))
               return (prefix ++ rand ++ suffix)
           )
           (\path -> fmap not (path_exists' path))

   where char nr = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" !! nr


-- Execute action until condition is met.
untilIO io cond = do
   res <- io
   u <- cond res
   if u then return res
        else untilIO io cond


{- | One entry of mount information. This is the same as @struct mntent@ from @\<mntent.h\>@. A list of these is
returned by the functions which read mount information.

See 'read_mounts', 'read_mtab', 'read_fstab'.
-}
data Mntent = Mntent { mnt_fsname :: String        -- ^ Device file (\"name of mounted file system\")
                     , mnt_dir :: String           -- ^ Mount point
                     , mnt_type :: String          -- ^ Which kind of file system (\"see mntent.h\")
                     , mnt_opts :: String          -- ^ Mount options (\"see mntent.h\")
                     , mnt_freq :: Int             -- ^ Dump frequency in days
                     , mnt_passno :: Int           -- ^ \"Pass number on parallel fsck\"
                     }
   deriving (Read, Show, Typeable, Eq)

{- | Read mount information. This is a front end to the @setmntent(3)@, @getmntent(3)@, @endmntent(3)@ system
library functions.

When the @setmntent@ call fails, the @errno@ value is converted to an @IOError@ and thrown.

See 'read_mtab', 'read_fstab'.
-}
read_mounts :: String                           -- ^ File to read (typically @\/etc\/mtab@ or @\/etc\/fstab@)
            -> IO [Mntent]                      -- ^ Mount information in that file
read_mounts path = do
   h <- withCString path $ \cpath ->
      withCString "r" $ \r ->
         {#call setmntent#} cpath r
   when (h == nullPtr) $
      throwErrno' "setmntent(3) in read_mounts" Nothing (Just path)
   mntent <- getmntent h []
   {#call endmntent#} h
   return mntent

   where
      getmntent h l = do
         ptr <- {#call getmntent as c_getmntent#} h
         if (ptr == nullPtr) then return l
                             else do mnt_fsname_str <- {#get mntent.mnt_fsname#} ptr >>= peekCString
                                     mnt_dir_str <- {#get mntent.mnt_dir#} ptr >>= peekCString
                                     mnt_type_str <- {#get mntent.mnt_type#} ptr >>= peekCString
                                     mnt_opts_str <- {#get mntent.mnt_opts#} ptr >>= peekCString
                                     mnt_freq_int <- fmap fromEnum $ {#get mntent.mnt_freq#} ptr
                                     mnt_passno_int <- fmap fromEnum $ {#get mntent.mnt_passno#} ptr
                                     getmntent h (l ++ [Mntent { mnt_fsname = mnt_fsname_str
                                                               , mnt_dir = mnt_dir_str
                                                               , mnt_type = mnt_type_str
                                                               , mnt_opts = mnt_opts_str
                                                               , mnt_freq = mnt_freq_int
                                                               , mnt_passno = mnt_passno_int
                                                               }])

{- | Get the currently mounted file systems.

>read_mtab = read_mounts "/etc/mtab"

See 'read_mounts'.
-}
read_mtab :: IO [Mntent]
read_mtab = read_mounts "/etc/mtab"


{- | Get the system wide file system table.

>read_fstab = read_mounts "/etc/fstab"

See 'read_mounts'.
-}
read_fstab :: IO [Mntent]
read_fstab = read_mounts "/etc/fstab"


-- Taken from the source code of the GHC 6 libraries (in System.Posix.Internals). It isn't exported from there.
-- "HsBase.h" belongs to the files which are visible to users of GHC, but it isn't documented. The comment at the
-- beginning says "Definitions for package `base' which are visible in Haskell land.".
foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_excl"   o_EXCL   :: CInt



-- | This is an interface to the POSIX @glob@ function, which does wildcard expansion
-- in paths. The sorted list of matched paths is returned. It's empty
-- for no match (rather than the original pattern). In case anything goes wrong
-- (such as permission denied), an IOError is thrown.
--
-- This does /not/ do tilde expansion, which is done (among many unwanted other
-- things) by @wordexp@. The only flag used for the call to @glob@ is @GLOB_ERR@.
--
-- The behaviour in case of non-existing path components is inconsistent in the
-- GNU version of the underlying @glob@ function. @glob "\/doesnt_exist\/foo"@ will return
-- the empty list, whereas @glob "\/doesnt_exist\/*"@ causes a "No such file or directory"
-- IOError.
--
-- Note that it isn't clear if dangling symlinks are matched by glob. From the
-- web: "Compared to other glob implementation (*BSD, bash, musl, and other
-- shells as well), GLIBC seems the be the only one that does not match dangling
-- symlinks. ... POSIX does not have any strict specification for dangling
-- symlinks". 
--
-- You will have to work around this problem, probably using
-- System.Directory.getDirectoryContents. 
-- 
-- See man pages @glob(3)@ and @wordexp(3)@.
glob :: String                  -- ^ Pattern
     -> IO [String]             -- ^ Sorted list of matching paths
glob pattern = do
   withCString pattern $ \pattern_ptr ->
      allocaBytes {#sizeof glob_t#} $ \buf_ptr ->
         do res <- {#call do_glob#} buf_ptr pattern_ptr
            case res of
               0 -> -- success
                    do pptr <- {#get glob_t->gl_pathv#} buf_ptr
                       len <- lengthArray0 nullPtr pptr
                       cstrs <- peekArray len pptr
                       mapM peekCString cstrs
               1 -> -- GLOB_ABORTED
                    throwErrno' "glob" Nothing (Just pattern)
               2 -> -- GLOB_NOSPACE
                    ioError (ioeSetErrorString (mkIOError ResourceExhausted "glob" Nothing (Just pattern))
                                               "Out of memory")
               3 -> -- GLOB_NOMATCH
                    return []
         `finally`
            (do pptr <- {#get glob_t->gl_pathv#} buf_ptr
                when (pptr /= nullPtr) $
                   {#call globfree#} buf_ptr
            )


-- |
-- Quote special characters for use with the @glob@ function.
--
-- The characters @*@, @?@, @[@ and @\\@ must be quoted by preceding
-- backslashs, when they souldn't have their special meaning. The @glob_quote@
-- function does this.
-- 
-- You can't use @quote@ or @shell_quote@.
--
-- See 'glob', 'HsShellScript.Shell.quote', 'HsShellScript.Shell.shell_quote'
glob_quote :: String
           -> String
glob_quote path = 
   case path of
      []          -> []
      ('*':rest)  -> ('\\':'*':glob_quote rest)
      ('?':rest)  -> ('\\':'?':glob_quote rest)
      ('[':rest)  -> ('\\':'[':glob_quote rest)
      ('\\':rest) -> ('\\':'\\':glob_quote rest)
      (ch:rest)   -> ch : glob_quote rest



#c
/*
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
*/
#include <mntent.h>
#include <sys/stat.h>
#include <glob.h>

int close(int fd);


/* open(2) is defined in fcntl.h as "extern int open (__const char *__file, int __oflag, ...)", with variable
   number of arguments, which isn's supported by the FFI.
*/
int hsshellscript_open_nonvariadic(const char *pathname, int flags, mode_t mode);

int do_glob(void* buf, const char* pattern);


#endc
