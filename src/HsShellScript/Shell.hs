-- #hide
module HsShellScript.Shell where

import Data.List
-- import System

-- |
-- Generate command (for a shell) which corresponds to the specified program
-- name and argument list. The program name and arguments are the usual
-- parameters for calling an external program, like when using
-- @runProcess@ or @run@. The generated shell command
-- would achieve the same effect. The name and the arguments are properly
-- quoted, using 'shell_quote'.
--
-- Note: The quoted strings are correctly recognized in shell scripts. But the shell bash has an annoying history
-- expansion \"feature\", which causes it to choke on exclamation marks, when in interactive mode, even when quoted
-- with double quotes. You can turn it off with @set +o histexpand@.
shell_command :: String         -- ^ name or path of the executable
              -> [String]       -- ^ command line arguments
              -> String         -- ^ shell command
shell_command k par =
    concat (intersperse " " (map shell_quote (k:par)))


-- |
-- Quote shell metacharacters.
--
-- This function quotes strings, such that they are not misinterpreted by
-- the shell. It tries to be friendly to a human reader - when special
-- characters are present, then the string is quoted with double quotes. If
-- not, it is left unchanged.
--
-- The list of exacly which characters need to be quoted has been taken
-- from the bash source code. Bash in turn, implements POSIX 1003.2. So the
-- result produced should be correct. From the bash info pages:
-- \"... the rules for evaluation and quoting are taken from the POSIX
-- 1003.2 specification for the `standard' Unix shell.\"
--
-- Note: The quoted strings are correctly recognized in shell scripts. But the shell bash has an annoying history
-- expansion \"feature\", which causes it to choke on exclamation marks, when in interactive mode, even when quoted
-- with double quotes. You can turn it off with @set +o histexpand@.
--
-- See 'quote'.
shell_quote :: String -> String
shell_quote "" = "\"\""
shell_quote txt =
   let need_to_quote c = c `elem` "' \t\n\"\\|&;()<>!{}*[?]^$`#"
   in if any need_to_quote txt
         then '"' : quote0' txt
         else txt
   where
      quote0' :: String -> String
      quote0' (z:zs) =
         if (z `elem` "\"$`\\") then ('\\':(z:(quote0' zs)))
                                else (z:(quote0' zs))
      quote0' "" = "\""

-- |
-- Quote special characters inside a string for the shell
--
-- This quotes special characters inside a string, such that it is
-- recognized as one string by the shell when enclosed in double quotes.
-- Doesn't add the double quotes.
--
-- Note: The quoted strings are correctly recognized in shell scripts. But the shell bash has an annoying history
-- expansion \"feature\", which causes it to choke on exclamation marks, when in interactive mode, even when quoted
-- with double quotes. You can turn it off with @set +o histexpand@.
--
-- See 'quote', 'shell_quote'.
quote0 :: String -> String
quote0 (z:zs) =
   if (z `elem` "\"$`\\") then ('\\':(z:(quote0 zs)))
                          else (z:(quote0 zs))
quote0 "" = ""

-- |
-- Quote a string for the shell
--
-- This encloses a string in double quotes and quotes any special
-- characters inside, such that it will be recognized as one string by a
-- shell. The double quotes are added even when they aren't needed for this
-- purpose.
--
-- Note: The quoted strings are correctly recognized in shell scripts. But the shell bash has an annoying history
-- expansion \"feature\", which causes it to choke on exclamation marks, when in interactive mode, even when quoted
-- with double quotes. You can turn it off with @set +o histexpand@.
--
-- See 'quote0', 'shell_quote'.
quote :: String -> String
quote str = "\"" ++ quote0 str ++ "\""
