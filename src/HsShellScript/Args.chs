-- |
-- This module provides a more convient way of parsing command line
-- arguments than the GHC GetOpt package. It builds on top of GetOpt, but hides
-- it from the user. It is reexported from module HsShellScript.
--
-- For each command line argument, a description is to be created with
-- @argdesc@. Then the command line arguments are evaluated with
-- one of the @getargs@... functions. In case of an error, this will cause a
-- exception, which provides an expressive error message to be
-- printed. Then the @arg@... functions are used to extract the
-- values contained in the arguments, with the right type. The typical use
-- of HsShellScript.Args looks something like this:
--
-- >import HsShellScript
-- >import Control.Exception
-- >import Control.Monad
-- >import System.Environment
-- >import System.Exit
-- >import System.IO
-- >
-- >header = "mclapep - My Command Line Argument Parser Example Program, version 1.0.0\n\n"
-- >descs  = [d_onevalue, d_values, d_switch {-...-}]
-- >
-- >d_onevalue = argdesc [ desc_short 'o', desc_at_most_once, desc_argname "a", desc_value_required {-...-}]
-- >d_values   = argdesc [ desc_direct, desc_any_times {-...-} ]
-- >d_switch   = argdesc [ desc_long "doit", desc_at_most_once {-...-} ]
-- >-- ...
-- >
-- >args = unsafe_getargs header descs
-- >val  = optarg_req args d_onevalue        -- val  :: Maybe String
-- >vals = args_req   args d_values          -- vals :: [String]
-- >doit = arg_switch args d_switch          -- doit :: Bool
-- >-- ...
-- >
-- >main = mainwrapper $ do
-- >   args0 <- getArgs
-- >   when (null args0) $ do
-- >      -- No command line arguments - print usage information
-- >      print_usage_info stdout header descs
-- >      exitWith ExitSuccess
-- >   -- trigger argument errors
-- >   seq args (return ())
-- >
-- >   -- Do something with the arguments
--
-- Errors in the argument descriptions are regarded as bugs, and handled
-- by aborting the program with a message which is meaningful to the
-- programmer. It is assumed that the argument description is a constant for
-- a given program.
--
-- Errors in the arguments are reported using HsShellScript's error handling
-- scheme. An error description
-- value is generated, and either returned via an @Either@
-- value, or thrown as an exception.

module HsShellScript.Args ( -- ** Argument Properties
                    ArgumentProperty (..)
                  , ArgumentDescription (..)
                  , ArgumentValueSpec (..)
                  , Argtester
                  , argdesc
                  , desc_short
                  , desc_long
                  , desc_direct
                  , desc_value_required
                  , desc_value_optional
                  , desc_times
                  , desc_once
                  , desc_at_least_once
                  , desc_at_most_once
                  , desc_any_times
                  , desc_at_least
                  , desc_at_most
                  , desc_argname
                  , desc_description
                  , desc_tester
                  , desc_integer
                  , desc_nonneg_integer
                  , readtester
                  , is_direct
                    -- ** Evaluating the Command Line
                  , Arguments
                  , getargs
                  , getargs_ordered
                  , getargs'
                  , getargs_ordered'
                  , unsafe_getargs
                  , unsafe_getargs_ordered
                    -- ** Extracting the Argument Values
                  , arg_switch
                  , arg_times
                  , args_opt
                  , args_req
                  , reqarg_opt
                  , reqarg_req
                  , optarg_opt
                  , optarg_req
                  , arg_occurs
                    -- ** Placing additional Constraints on the Arguments
                  , args_none
                  , args_all
                  , args_one
                  , args_at_most_one
                  , args_at_least_one
                  , arg_conflicts
                    -- ** Argument Error Reporting
                  , ArgError (..)
                  , usage_info
                  , make_usage_info
                  , print_usage_info
                  , argname
                  , argname_a
                  , argname_short
                  , argname_long
                  , wrap
                  ) where

-- We use a fixed copy of GHC's GetOpt implementation. This is to work around a bug.
-- import System.Console.GetOpt
import HsShellScript.GetOpt
import {-# SOURCE #-} HsShellScript.ProcErr (terminal_width, terminal_width_ioe)

import Foreign.C
import System.Environment
import Control.Monad
import Control.Exception
import Prelude hiding (catch)
import Data.Maybe
import System.Environment
import Data.List
import GHC.IO
import System.IO
import HsShellScript.Shell
import Data.Char
import Debug.Trace
import Data.Typeable
import Control.Concurrent.MVar



-- | Does the command line argument take an value?
data ArgumentValueSpec  = ArgumentValue_none     -- ^ No value
                        | ArgumentValue_required -- ^ Value required
                        | ArgumentValue_optional -- ^ Value optional
   deriving (Eq, Show, Ord)


-- | Argument value tester function. This tests the format of an argument's value for errors. The tester function
-- is specified by 'desc_tester' or such, as part of the argument description.
--
-- The tester is passed the argument value. If the format is correct, then it returns @Nothing@. If there is an
-- error, then it returns @Just msgf@, with @msgf@ being an error message generation function. This function gets
-- passed the argument description, and produces the error message. The argument description typically is used to
-- extract a descriptive name of the argument (using 'argname' or 'argname_a') to be included in the error message.
type Argtester = String                           -- Argument value to be tested
                 -> Maybe (ArgumentDescription    -- Argument description for message generation
                           -> String              -- Error message
                          )


-- | Description of one command line argument. These are generated by
-- @argdesc@ from a list of argument properties, and subsequently used by one of the
-- @getargs@... functions.

data ArgumentDescription = ArgumentDescription {
        argdesc_short_args :: [Char],             -- ^ Short option names
        argdesc_long_args :: [String],            -- ^ Long option names
        argdesc_argarg :: ArgumentValueSpec,      -- ^ What about a possible value of the argument?
        argdesc_times :: Maybe (Int,Int),         -- ^ Minimum and maximum of number of occurences allowed
        argdesc_argargname :: Maybe String,       -- ^ Name for argument's value, for message generation
        argdesc_description :: Maybe String,      -- ^ Descrition of the argument, for message generation
        argdesc_argarg_tester :: Maybe Argtester  -- ^ Argument value tester
      }

-- excluding tester
ad_tup ad =
   (argdesc_short_args ad, argdesc_long_args ad, argdesc_argarg ad, argdesc_times ad,
    argdesc_argargname ad, argdesc_description ad)

instance Eq ArgumentDescription where
   d == e = ad_tup d == ad_tup e

instance Ord ArgumentDescription where
   compare d e = compare (ad_tup d) (ad_tup e)

-- value for maximum number of times
unlimited = -1


-- Whether two argument descriptions describe the same argument.
-- Every short or long argument name occurs in only one argument
-- descriptor (this is checked). Every argument has a short or a long
-- name (short = [], long = [""] for direct arguments).

same_arg :: ArgumentDescription -> ArgumentDescription -> Bool
same_arg arg1 arg2 =
   case (argdesc_short_args arg1, argdesc_short_args arg2) of
      (a:_, b:_) -> a == b
      ([], [])   -> case (argdesc_long_args arg1, argdesc_long_args arg2) of
                       ([],_)  -> unnamed
                       (_,[])  -> unnamed
                       (l1,l2) -> head l1 == head l2
      _          -> False
   where unnamed = error "Bug in argument description: nameless, non-direct argument. \
                         \desc_short or desc_long must be specified."


-- | A property of a command line argument. These are generated by the
-- @desc_@... functions, and condensed to argument
-- descriptions of type @ArgumentDescription@ by @argdesc@. This type is abstract.
newtype ArgumentProperty =
   ArgumentProperty { argumentproperty :: ArgumentDescription -> ArgumentDescription }
-- An argument property is a function which fills in part of an argument descriptor.


-- starting value for argument descriptor
nulldesc :: ArgumentDescription
nulldesc =
   ArgumentDescription {
      argdesc_short_args = [],
      argdesc_long_args = [],
      argdesc_argarg = ArgumentValue_none,
      argdesc_times = Nothing,          -- default = (0,1)
      argdesc_argargname = Nothing,
      argdesc_description = Nothing,
      argdesc_argarg_tester = Nothing
   }

-- default number of times an argument may occur
times_default = (0,1)


-- | This represents the parsed contents of the command line. It is returned
-- by the @getargs@... functions, and passed on to the
-- value extraction functions by the user.
--
-- See 'getargs', 'getargs_ordered', 'getargs\'', 'getargs_ordered\''.
newtype Arguments =
    Arguments ([ ( ArgumentDescription             -- argument descriptor
                 , [Maybe String]                  -- arguments matching this descriptor
                 )],
               String)                             -- header


argvalues :: Arguments -> ArgumentDescription -> [Maybe String]
argvalues (Arguments (l, header)) desc =
   argvalues' l
   where
      argvalues' ((d,v):r) = if same_arg desc d then v else argvalues' r
      argvalues' []        = abort "Bug using HsShellScript: Value of unknown argument queried \
                                   \(add it to getarg's list)" desc

-- used internally to represent one occurence of a specific argument
type ArgOcc = (ArgumentDescription, Maybe String)


-- | Error thrown when there is an error in the command line arguments.
--
-- The usage information is generated by the deprecated function usage_info. Better ignore this, and use the newer
-- @make_usage_info@ or @print_usage_info@.
--
-- See 'make_usage_info', 'print_usage_info', 'usage_info'.
data ArgError = ArgError {
      -- | Error message
      argerror_message :: String,
      -- | Deprecated. Usage information, as generated by the now deprecated function 'usage_info'.
      argerror_usageinfo :: String
   }
   deriving (Typeable)


argerror_ui :: String
            -> [ArgumentDescription]
            -> a
argerror_ui mess descl =
   throw (ArgError mess (make_usage_info1 descl))


-- | Make @ArgError@ an instance of @Exception@, so we can throw and catch it, using GHC-6.10\'s new exception
-- library.
instance Exception ArgError


---
-- Printing an @ArgError@ will produce the error message. The usage
-- information must be printed separately, using @usage_info@.
instance Show ArgError where
   show argerror = argerror_message argerror


-- |
-- Whether the specified argument is the direct argument. Direct arguments are
-- the ones which are specified without introducing "-" or "--", in the command
-- line, or which occur after the special argument "--".
--
-- See 'argdesc', 'desc_direct'.
is_direct :: ArgumentDescription        -- ^ Argument description, as returned by @argdesc@.
          -> Bool
is_direct desc =
   argdesc_short_args desc == [] && argdesc_long_args desc == [""]


-- |
-- Short name of the argument. This specifies a character for a
-- one letter style argument, like @-x@. There can be specified
-- several for the same argument. Each argument needs at least
-- either a short or a long name.
desc_short :: Char                -- ^ The character to name the argument.
           -> ArgumentProperty    -- ^ The corresponding argument property.
desc_short c = ArgumentProperty
   (\desc ->
      if (c `elem` (argdesc_short_args desc))
         then abort ("Bug in HsShellScript argument description: Duplicate short argument " ++ show c ++ " specified") desc
         else if ("" `elem` argdesc_long_args desc)
                 then abort_conflict "" desc
                 else desc { argdesc_short_args = c : argdesc_short_args desc }
   )

-- | Long name of the argument. This specifies a GNU style long name for the argument, which is introduced by two
-- dashes, like @--arg@ or @--arg=...@. There can be specified several names for the same argument. Each argument
-- needs at least either a short or a long name. Except for direct arguments, which don't have a name.
--
-- See 'desc_direct'
desc_long :: String                     -- ^ The long name of the argument.
          -> ArgumentProperty     -- ^ The corresponding argument property.
desc_long str = ArgumentProperty
   (\desc ->
      if (str `elem` (argdesc_long_args desc))
         then abort ("Bug in HsShellScript argument description: Duplicate long argument " ++ show str ++ " specified") desc
         else if ("" `elem` argdesc_long_args desc)
                 then abort_conflict "" desc
                 else desc { argdesc_long_args = str : argdesc_long_args desc }
   )

-- |
-- Signal that this is the description of direct arguments. Direct arguments are the ones not
-- introduced by any short or long argument names (like @-x@ or @--arg@). After the special argument
-- @--@, also everything is a direct argument, even when starting with @-@ or @--@. The presence of
-- @desc_direct@ in the argument properties list signals @argdesc@ that this is the description of
-- the direct arguments. There may be at most one such description.
--
-- The @is_direct@ function can be used in order to determine if a specific
-- argument is the direct argument.
--
-- See 'is_direct'.
desc_direct :: ArgumentProperty
desc_direct = ArgumentProperty
   (\desc ->
      if argdesc_long_args desc == [] && argdesc_short_args desc == [] && argdesc_argarg desc == ArgumentValue_none
         then desc { argdesc_long_args = [""]
                   , argdesc_argarg = ArgumentValue_required
                   , argdesc_argargname = Just ""
                   }
         else abort_conflict "desc_direct conflicts with desc_long, desc_short, desc_value_required \
                             \and desc_value_optional." desc
   )

-- |
-- Signal that the argument requires a value.
desc_value_required :: ArgumentProperty
desc_value_required = ArgumentProperty
   (\desc ->
      if argdesc_argarg desc == ArgumentValue_none
         then desc { argdesc_argarg = ArgumentValue_required }
         else abort_conflict "desc_value_required repeated or conflicting desc_value_optional" desc
   )

-- |
-- Signal that the argument optionally has a value. The user may or may
-- not specify a value to this argument.
desc_value_optional :: ArgumentProperty
desc_value_optional = ArgumentProperty
   (\desc ->
      if argdesc_argarg desc == ArgumentValue_none
         then desc { argdesc_argarg = ArgumentValue_optional }
         else abort_conflict "desc_value_optional repeated or conflicting desc_value_required" desc
   )

-- |
-- Specify lower and upper bound on the number of times an argument may
-- occur.
desc_times :: Int                       -- ^ Lower bound of the allowed number of argdesc_times.
           -> Int                       -- ^ Upper bound of the allowed number of argdesc_times.
           -> ArgumentProperty          -- ^ The corresponding argument property.
desc_times n m = ArgumentProperty
   (\desc ->
       if argdesc_times desc == Nothing
          then desc { argdesc_times = Just (n,m) }
          else abort_conflict "desc_times conflicting previous number of occurences specification" desc
   )

-- |
-- Signal that the argument must be present exactly once. This is
-- meaningful only for arguments which can take a value.
desc_once :: ArgumentProperty     -- ^ The corresponding argument property.
desc_once = desc_times 1 1

-- |
-- Signal that the argument must occur at least one time.
desc_at_least_once :: ArgumentProperty -- ^ The corresponding argument property.
desc_at_least_once = desc_times 1 unlimited

-- |
-- Signal that the argument must occur at most one time.
desc_at_most_once :: ArgumentProperty -- ^ The corresponding argument property.
desc_at_most_once  = desc_times 0 1

-- |
-- Signal that the argument must have at least the specified number of
-- occurences, and has no upper limit of occurences.
desc_at_least :: Int                        -- ^ Number of times.
              -> ArgumentProperty           -- ^ The corresponding argument property.
desc_at_least n = desc_times n unlimited

-- |
-- Signal that the argument may occur any number of times.
desc_any_times :: ArgumentProperty -- ^ The corresponding argument property.
desc_any_times  = desc_times 0 unlimited

-- |
-- Signal that the argument does not need to be present, and may occur at most
-- the specified number of times.
desc_at_most :: Int                     -- ^ Number of times.
             -> ArgumentProperty  -- ^ The corresponding argument property.
desc_at_most n = desc_times 0 n

-- |
-- Specify the descriptive name for command line argument's value. Used for the
-- generation of the usage message. The name should be very short.
desc_argname :: String                          -- ^ Name of the argument's value.
             -> ArgumentProperty          -- ^ The corresponding argument property.
desc_argname name = ArgumentProperty
   (\desc ->
      if argdesc_argargname desc == Nothing
         then desc { argdesc_argargname = Just name }
         else abort "Bug in HsShellScript argument description: Multiple names specified" desc
   )

-- |
-- Specify a description of what the argument does. Used for usage message
-- generation. This can be arbitratily long, long lines are wrapped.
desc_description :: String                      -- ^ Short description of the argument.
                 -> ArgumentProperty            -- ^ The corresponding argument property.
desc_description expl = ArgumentProperty
   (\desc ->
      if argdesc_description desc == Nothing
         then desc { argdesc_description = Just expl }
         else abort "Bug in HsShellScript argument description: Multiple argument descriptions specified" desc
   )

-- | Specify a tester for this argument. The tester is a function which tests the argument value for format errors.
-- Typically, it tests whether the value can be parsed to some target type. If the test fails, the tester produces
-- an error message. When parsing the command line arguments (which @getargs@ or related), all the testers are
-- applied to the respective argument values, and an 'ArgError' is thrown in case of failure. By using a tester, it
-- can be ensured that the argument values abide a specific format when extracting them, such that they can be
-- parsed without errors, e.g. @myarg = read (reqarg_req args d_myarg)@.
--
-- An argument tester is a function of type 'Argtester'.
--
-- See 'readtester', 'desc_integer', 'desc_nonneg_integer', 'Argtester'.
desc_tester :: Argtester                        -- ^ Argument tester to apply to this argument
            -> ArgumentProperty                 -- ^ The corresponding argument property.
desc_tester t = ArgumentProperty
   (\desc ->
      case argdesc_argarg_tester desc of
         Nothing -> desc { argdesc_argarg_tester = Just t }
         Just _  -> abort "Bug in HsShellScript argument description: Multiple argument value testers specified"
                          desc
   )


-- | Build an argument tester from a @reads@ like function. Typically, a specialisation of the standard prelude
-- function @read@ is used. Example: @readtester \"Integer expected.\" (reads :: ReadS Int)@
readtester :: ReadS a                           -- Reader function, like the standard prelude function @reads@
           -> String                            -- Additional message
           -> Argtester                         -- Argument tester to be passed to 'desc_tester'
readtester reader msg val =
   case filter ((== "") . snd) $ reader val of
      [(_,"")] -> Nothing
      []       -> Just (\arg -> "Format error in the value of the " ++ argname_a arg ++ ". " ++ msg ++
                                "\nValue: " ++ quote val)
      _        -> Just (\arg -> "Ambigious value of the " ++ argname_a arg ++ ". " ++ msg ++ "\nValue: " ++
                                quote val)


{- | Specify that the value of this argument, if present, is a positive integer. This will cause an error when the
command line is parsed, and the argument's value doesn't specify an integer.

>desc_integer = desc_tester (readtester (reads :: ReadS Int) "Integer expected.")

   See 'desc_tester'.
-}
desc_integer :: ArgumentProperty
desc_integer = desc_tester (readtester (reads :: ReadS Int) "Integer expected.")


{- | Specify that the value of this argument, if present, is a non-negative integer. This will cause an error when
the command line is parsed, and the value doesn't specify a non-negative integer.

>desc_nonneg_integer = desc_tester (readtester ((filter (\(a,_) -> a >= 0) . reads) :: ReadS Int) \
>                                   "Non-negative integer expected." )

   See 'desc_tester'.
-}
desc_nonneg_integer :: ArgumentProperty
desc_nonneg_integer = desc_tester (readtester ((filter (\(a,_) -> a >= 0) . reads) :: ReadS Int)
                                   "Non-negative integer expected." )


abort_conflict msg = abort ("Conflicting properties in argument description. " ++ msg)
abort msg desc = error (msg ++ "\nargument (so far): " ++ argname desc)

-- |
-- Generate a descriptive argument name from an argument description, suitable
-- for use in error messages and usage information. This uses the long and short
-- argument names (as specified by 'desc_short' and 'desc_long') and generates
-- descriptive names of the argument like \"-f\", \"--myflag\",
-- \"-f\/--myflag\", etc. All the argument names are included. In case of direct
-- arguments (see 'desc_direct'), the descriptive name is \"@(direct
-- argument)@\".
--
-- See 'argdesc'.
argname :: ArgumentDescription  -- ^ Argument description, as returned by @argdesc@
        -> String               -- ^ Printable name for the argument
argname desc =
   if (argdesc_short_args desc, argdesc_long_args desc) == ([],[""]) then "(direct argument)"
      else if (argdesc_short_args desc, argdesc_long_args desc) == ([],[]) then "yet unnamed argument"
         else concat (intersperse "/" ( map (\s -> "-"++[s]) (argdesc_short_args desc) ++
                                        map ("--" ++) (argdesc_long_args desc) ))

-- |
-- Generate a descriptive argument name from an argument description, beginning
-- with \"argument\". This uses the long and short argument names (as specified
-- by 'desc_short' and 'desc_long') and generates descriptive names of the
-- argument like \"argument -f\", \"argument --myflag\", \"argument
-- -f\/--myflag\", etc. All the argument names are included. In case of direct
-- arguments (see 'desc_direct'), the descriptive name is \"direct argument\".
--
-- See 'argdesc'.
argname_a :: ArgumentDescription        -- ^ Argument description, as returned by @argdesc@
          -> String                     -- ^ Printable name for the argument
argname_a desc =
   if (argdesc_short_args desc, argdesc_long_args desc) == ([],[""]) then "direct argument"
      else if (argdesc_short_args desc, argdesc_long_args desc) == ([],[]) then "yet unnamed argument"
         else "argument " ++ concat (intersperse "/" ( map (\s -> "-"++[s]) (argdesc_short_args desc) ++ map ("--" ++) (argdesc_long_args desc) ))



{- | Create a string, which lists the short forms of one command line argument. If
it has an subargument, it's name is listed as well. For arguments without short
form, the result is the empty string.

For the illegal command line argument, with neither short nor long forms, and
not being the direct argument either, the result is @"yet unnamed argument"@.
Such argument descriptions are incomplete, and will be rejected by @getargs@ and
@unsafe_getargs@.

This is meant for refering to an argument, such as in error messages or usage
information.

Examples:

>argname_short (argdesc [ desc_short 'a'
>                       , desc_short 'b'
>                       , desc_value_required
>                       , desc_argname "Name"
>                       ])
>  == "-a/-b Name"

See 'argdesc', 'desc_direct'. 'argname_long'.
-}
argname_short :: ArgumentDescription  -- ^ Argument description, as returned by @argdesc@
              -> String               -- ^ Printable name for the argument
argname_short desc =
   if (argdesc_short_args desc, argdesc_long_args desc) == ([],[""])
   then ""
   else if (argdesc_short_args desc, argdesc_long_args desc) == ([],[])
        then "yet unnamed argument"
        else
           case (argdesc_short_args desc, argdesc_argargname desc) of
              ([], _)         -> ""
              (sl, Just name) -> concat (intersperse "/" (map (\s -> "-" ++ [s]) sl)) ++ " " ++ name
              (sl, Nothing)   -> concat (intersperse "/" (map (\s -> "-" ++ [s]) sl))



{- | Create a string, which lists the long forms of one command line argument. If
it has an subargument, it's name is listed as well. For arguments without long
form, the result is the empty string.

For the illegal command line argument, with neither short nor long forms, and
not being the direct argument either, the result is @"yet unnamed argument"@.
Such argument descriptions are incomplete, and will be rejected by @getargs@ and
@unsafe_getargs@.

This is meant for refering to an argument, such as in error messages or usage
information.

Examples:

>argname_long (argdesc [ desc_long "foo"
>                      , desc_long "bar"
>                      , desc_value_required
>                      , desc_argname "Name"
>                      ])
>  == "--foo/--bar Name"

See 'argdesc', 'desc_direct'. 'argname_long'.
-}
argname_long :: ArgumentDescription  -- ^ Argument description, as returned by @argdesc@
             -> String               -- ^ Printable name for the argument
argname_long desc =
   if (argdesc_short_args desc, argdesc_long_args desc) == ([],[""])
   then ""
   else if (argdesc_short_args desc, argdesc_long_args desc) == ([],[])
        then "yet unnamed argument"
        else
           case (argdesc_long_args desc, argdesc_argargname desc) of
              ([], _)         -> ""
              (sl, Just name) -> concat (intersperse "/" (map (\s -> "--" ++ s) sl)) ++ " " ++ name
              (sl, Nothing)   -> concat (intersperse "/" (map (\s -> "--" ++ s) sl))



up1 "" = ""
up1 (x:xs) = toUpper x : xs

-- complete generation of argument description
prop_final :: ArgumentProperty
prop_final = ArgumentProperty
   (\desc ->
      seq (if argdesc_argarg desc /= ArgumentValue_none && argdesc_argargname desc == Nothing
              then error $ "Bug in description of " ++ argname_a desc ++ ": Argument's value must be given a name using desc_argname."
              else if argdesc_argarg desc == ArgumentValue_none && argdesc_argargname desc /= Nothing
                      then error $ "Bug in description of " ++ argname_a desc
                           ++ ": Argument doesn't take a sub argument, but a name for it is specified."
                      else ()
          ) $
          desc { argdesc_times = Just (fromMaybe times_default (argdesc_times desc))
               , argdesc_description = Just (fromMaybe "" (argdesc_description desc))
               }
   )


-- |
-- Make an argument description from a list of argument properties. This
-- condenses the list to an argument description,
-- which can be used by the @getargs@... functions and the
-- argument value extraction functions.

argdesc :: [ArgumentProperty]     -- ^ List of properties, which describe the command line argument.
        -> ArgumentDescription    -- ^ The corresponding argument description.
argdesc propl =
   foldr (.) id (map argumentproperty (prop_final:propl)) nulldesc


-- Parse command line arguments.
getargs0 :: String                      -- Header for usage info
         -> ArgOrder ArgOcc             -- HsShellScript.GetOpt.Permute or HsShellScript.GetOpt.RequireOrder
                                        --   Permute:      Named arguments (like -x or --arg) and direct arguments
                                        --                 may occur in any order.
                                        --   RequireOrder: All arguments after the first direct argument are
                                        --                 regarded as direct arguments.
         -> [String]                    -- The command line arguments as returned by System.Environment.getArgs
         -> [ArgumentDescription]       -- The arguments descriptions
         -> Either ArgError             -- Error
                   Arguments            -- Parsed command line arguments
getargs0 header ordering cmdlargs descs =
   let (  descs_direct     -- direct arguments (without argument name)
        , descs_regular    -- regular arguments (with long or short argument name)
        ) = partition is_direct descs

       nonunique :: Eq a => [a] -> Maybe a
       nonunique (a:b:r) = if (a == b) then (Just a) else nonunique (b:r)
       nonunique _       = Nothing

       test_unique :: (Show a, Ord a) => (ArgumentDescription -> [a]) -> String -> b -> b
       test_unique extr what x =
           case nonunique (sort (concat (map extr descs))) of
              Just y -> error ("Bug: Several occurences of " ++ what ++ " " ++ show y ++
                               " in command line argument specifications")
              Nothing -> x

       optdescr = map make_optdescr descs_regular

       make_optdescr :: ArgumentDescription -> OptDescr ArgOcc
       make_optdescr desc =
          Option (argdesc_short_args desc)
                 (argdesc_long_args desc)
                 (case argdesc_argarg desc of
                     ArgumentValue_none      -> NoArg  (desc, Nothing)
                     ArgumentValue_required     -> ReqArg (\arg -> (desc, Just arg))
                                              (fromJust (argdesc_argargname desc))
                     ArgumentValue_optional     -> OptArg (\arg -> (desc, arg))
                                              (fromJust (argdesc_argargname desc))
                 )
                 (fromJust (argdesc_description desc))

       -- Postprocessing after successful call to getOpt
       getopt_post :: [ArgOcc] -> [String] -> Either ArgError Arguments
       getopt_post pars{-getOpt recognized arguments-} rest{-direct arguments-} =
          case (rest, descs_direct) of
             ([],[])  ->
                -- no direct arguments allowed and none provided
                getopt_post' pars
             (r, [d]) ->
                -- direct arguments allowed and expected
                getopt_post' (pars ++ zip (repeat d) (map Just r))
             ((x:xs), []) ->
                -- direct arguments provided, but not allowed
                Left (ArgError "Surplus arguments."
                               (make_usage_info1 descs)
                     )
             _ ->
                -- several descriptions for direct arguments
                error "Bug in argument descriptions: Several descriptions for direct arguments \
                      \(desc_direct) specified."

       add :: (ArgumentDescription, Maybe String)
           -> [(ArgumentDescription, [Maybe String])]
           -> [(ArgumentDescription, [Maybe String])]
       add (a,str) []        = [(a,[str])]
       add (b,str) ((a,l):r) =
          if same_arg a b then (a,str:l) : r
                          else (a,l) : add (b,str) r

       getopt_post' :: [ArgOcc] -> Either ArgError Arguments
       getopt_post' pars{-all arguments-} =
          let pars' = foldr add (map (\d -> (d,[])) descs) pars

              -- Check the number of argument occurences
              check_num :: [(ArgumentDescription, [Maybe String])] -> Maybe ArgError
              check_num [] = Nothing
              check_num ((desc,args):rest) =
                 let (min,max) = fromJust (argdesc_times desc)
                     number    = length args
                     wrong_number_msg =
                        (if is_direct desc then fst else snd) $
                        if number == 0 && min == 1 then
                           ( "Missing argument."
                           , "Missing " ++ argname_a desc ++ "."
                           )
                        else if number < min then
                           ( "Too few arguments. " ++ show min ++ " required."
                           , "Too few instances of " ++ argname_a desc ++ ". "++ show min ++ " required."
                           )
                        else if number > max && max == 1 then
                           ( "Only one argument may be specified."
                           , "Repeated " ++ argname_a desc ++ "."
                           )
                        else if number > max && max /= unlimited then
                           ( "Too many arguments."
                           , "Too many instances of " ++ argname_a desc ++ "."
                           )
                        else error "bug in HsShellScript.Args.hs"
                 in  if number >= min && (number <= max || max == unlimited)
                        then check_num rest
                        else Just (ArgError wrong_number_msg (make_usage_info1 descs))

              -- Apply any argument testers
              check_testers :: [(ArgumentDescription, [Maybe String])] -> Maybe ArgError
              check_testers [] = Nothing
              check_testers ((desc,args):rest) =
                 case argdesc_argarg_tester desc of
                    Just argdesc_argarg_tester ->
                       if argdesc_argarg desc == ArgumentValue_none
                          then abort "Bug in HsShellScript argument descriptions: Argument value tester \
                                     \specified,\n\
                                     \but no argument value has been allowed. Add desc_value_optional or\n\
                                     \desc_value_required."
                                     desc
                          else case filter isJust (map (argdesc_argarg_tester . fromJust) (filter isJust args)) of
                                  []              -> check_testers rest
                                  (Just msgf : _) -> Just (ArgError (msgf desc) (make_usage_info1 descs))
                    Nothing -> check_testers rest

          in  case check_testers pars' of
                 Nothing  -> case check_num pars' of
                                Nothing  -> Right (Arguments (pars', header))
                                Just err -> Left err
                 Just err -> Left err

       args =
          test_unique argdesc_short_args "short argument" $
             test_unique argdesc_long_args "long argument" $
                case getOpt ordering optdescr cmdlargs of
                   (pars, rest, []) ->
                      getopt_post pars rest
                   (_,_,f) ->
                      throw (ArgError (unlines (map chomp f)) (usageInfo header optdescr))

    in args

   where
      -- duplicated here in order to break cyclic module dependency
      chomp "" = ""
      chomp "\n" = ""
      chomp [x] = [x]
      chomp (x:xs) = let xs' = chomp xs
                     in  if xs' == "" && x == '\n' then "" else x:xs'




-- |
-- Parse command line arguments. The arguments are taken from a call to
-- @getArgs@ and parsed. Any error is thrown as a
-- @ArgError@ exception. The result is a value from which the
-- information in the command line can be extracted by the @arg@...,
-- @reqarg@... and @optarg@... functions.
--
-- The header is used only by the deprecated @usage_info@ function. If you don't
-- use it, you don't need to specify a header. Just pass an empty string.
--
-- Named arguments (like @-x@ or @--arg@) and direct
-- arguments may occur in any order.
--
-- See 'usage_info', 'make_usage_info', 'print_usage_info'.
getargs :: String                         -- ^ Header to be used by the deprecated @usage_info@ function.
        -> [ArgumentDescription]          -- ^ The argument descriptions.
        -> IO Arguments                   -- ^ The contents of the command line.
getargs header descs = do
   args <- getArgs
   let res = getargs0 header Permute args descs
   either throw
          return
          res

-- |
-- Parse command line arguments. The arguments are taken from a call to
-- @getArgs@ and parsed. Any error is thrown as a
-- @ArgError@ exception. The result is a value from which the
-- information in the command line can be extracted by the @arg@...,
-- @reqarg@... and @optarg@... functions.
--
-- The header is used only by the deprecated @usage_info@ function. If you don't
-- use it, you don't need to specify a header. Just pass an empty string.
--
-- All arguments after the first direct argument are regarded as direct
-- arguments. This means that argument names introduced by @-@
-- or @--@ no longer take effect.
--
-- See 'usage_info', 'make_usage_info', 'print_usage_info'.
getargs_ordered :: String                 -- ^ Header to be used by the deprecated @usage_info@ function.
                -> [ArgumentDescription]  -- ^ Descriptions of the arguments.
                -> IO Arguments           -- ^ The contents of the command line.
getargs_ordered header descs = do
   args <- getArgs
   either throw
          return
          (getargs0 header RequireOrder args descs)

-- |
-- Parse the specified command line. Any error is returned as @Left
-- argerror@. In case of success, the result is returned as
-- @Right res@. From the result, the information in the command
-- line can be extracted by the @arg@..., @reqarg@...
-- and @optarg@... functions.
--
-- The header is used only by the deprecated @usage_info@ function. If you don't
-- use it, you don't need to specify a header. Just pass an empty string.
--
-- Named arguments (like @-x@ or @--arg@) and direct
-- arguments may occur in any order.
--
-- See 'usage_info', 'make_usage_info', 'print_usage_info'.
getargs' :: String                              -- ^ Header to be used by the deprecated @usage_info@ function.
         -> [String]                            -- ^ Command line to be parsed.
         -> [ArgumentDescription]         -- ^ The argument descriptions.
         -> Either ArgError Arguments     -- ^ The contents of the command line.
getargs' header args descs = getargs0 header Permute args descs

-- |
-- Parse the specified command line. Any error is returned as @Left
-- argerror@. In case of success, the result is returned as
-- @Right res@. From the result, the information in the command
-- line can be extracted by the @arg@..., @reqarg@...
-- and @optarg@... functions.
--
-- The header is used only by the deprecated @usage_info@ function. If you don't
-- use it, you don't need to specify a header. Just pass an empty string.
--
-- All arguments after the first direct argument are regarded as direct
-- arguments. This means that argument names introduced by @-@
-- or @--@ no longer take effect.
--
-- See 'usage_info', 'make_usage_info', 'print_usage_info'.
getargs_ordered' :: String                        -- ^ Header to be used by the deprecated @usage_info@ function.
                 -> [String]                      -- ^ Command line to be parsed.
                 -> [ArgumentDescription]         -- ^ The argument descriptions.
                 -> Either ArgError Arguments     -- ^ The contents of the command line.
getargs_ordered' header args descs = getargs0 header RequireOrder args descs


test_desc :: ArgumentDescription -> Bool -> String -> b -> b
test_desc desc ok msg x =
   if ok then x
         else abort msg desc

maybe_head :: [a] -> Maybe a
maybe_head [] = Nothing
maybe_head [a] = Just a

-- |
-- Query whether a certain switch is specified on the command line. A switch is an
-- argument which is allowed zero or one time, and has no value.
arg_switch :: Arguments                   -- ^ Command line parse result.
           -> ArgumentDescription         -- ^ Argument description of the switch.
           -> Bool                              -- ^ Whether the switch is present in the command line.
arg_switch args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_none && argdesc_times desc == Just (0,1))
             "bug: querying argument with is not a switch with arg_switch" $
   case argvalues args desc of
      []         -> False
      [Nothing]  -> True

-- |
-- Query the number of occurences of an argument.
arg_times :: Arguments                    -- ^ Command line parse result.
          -> ArgumentDescription          -- ^ Description of the argument.
          -> Int                          -- ^ Number of times the argument occurs.
arg_times args desc =
   length (argvalues args desc)

-- |
-- Query the values of an argument with optional value. This is for
-- arguments which take an optional value, and may occur several times. The
-- occurences with value are represented as @Just value@, the occurences
-- without are represented as @Nothing@.
args_opt :: Arguments                     -- ^ Command line parse result.
         -> ArgumentDescription           -- ^ Description of the argument.
         -> [Maybe String]                      -- ^ The occurences of the argument.
args_opt args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_optional && snd (fromJust (argdesc_times desc)) /= 1)
             "Bug: Querying argument which doesn't take an optional value, or may not occur several times, \
             \with args_opt."
   $ argvalues args desc

-- |
-- Query the values of an argument with required value. This is for
-- arguments which require a value, and may occur several times.
args_req :: Arguments                     -- ^ Command line parse result.
         -> ArgumentDescription           -- ^ Description of the argument.
         -> [String]                            -- ^ The values of the argument.
args_req args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_required && snd (fromJust (argdesc_times desc)) /= 1)
             "Bug: Querying argument which doesn't require a value, or may not occur several times, with \
             \args_req." $
   map fromJust (argvalues args desc)

-- |
-- Query the optional value of a required argument. This is for arguments
-- which must occur once, and may have a value. If the argument is
-- specified, its value is returned as @Just value@. If it isn't, the result
-- is @Nothing@.
reqarg_opt :: Arguments                   -- ^ Command line parse result.
           -> ArgumentDescription         -- ^ Description of the argument.
           -> Maybe String                      -- ^ The value of the argument, if it occurs.
reqarg_opt args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_optional && argdesc_times desc == Just (1,1))
             "Bug: Querying argument which doesn't take an optional value, or which must not occur exactly \
             \once, with reqarg_opt." $
   head (argvalues args desc)


-- |
-- Query the value of a required argument. This is for arguments which must
-- occur exactly once, and require a value.
reqarg_req :: Arguments                   -- ^ Command line parse result.
           -> ArgumentDescription         -- ^ Description of the argument.
           -> String                            -- ^ The value of the argument.
reqarg_req args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_required && argdesc_times desc == Just (1,1))
             "Bug: Querying argument with non-required value, or which doesn't occur exactly once, with reqarg_req." $
   fromJust (head (argvalues args desc))

-- |
-- Query the optional value of an optional argument. This is for arguments
-- which may occur zero or one time, and which may or may not have a value.
-- If the argument doesn't occur, the result is @Nothing@. If it does occur,
-- but has no value, then the result is @Just Nothing@. If it does occur with
-- value, the result is @Just (Just value)@.
optarg_opt :: Arguments                   -- ^ Command line parse result.
           -> ArgumentDescription         -- ^ Description of the argument.
           -> Maybe (Maybe String)              -- ^ The occurence of the argument and its value (see above).
optarg_opt args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_optional)
             "Bug: Querying argument with non-optional value with optarg_opt." $
   test_desc desc (fst (fromJust (argdesc_times desc)) == 0)
             "Bug: Querying argument which isn't optional with optarg_opt." $
   test_desc desc (snd (fromJust (argdesc_times desc)) == 1)
             "Bug: Querying argument which may occur several times optarg_opt." $
   maybe_head (argvalues args desc)


-- |
-- Query the value of an optional argument. This is for optional arguments
-- which require a value, and may occur at most once. The result is
-- @Just value@ if the argument occurs, and @Nothing@
-- if it doesn't occur.
optarg_req :: Arguments                   -- ^ Command line parse result.
           -> ArgumentDescription         -- ^ Description of the argument.
           -> Maybe String                      -- ^ The value of the argument, if it occurs.
optarg_req args desc =
   test_desc desc (argdesc_argarg desc == ArgumentValue_required)
             "Bug: Querying argument with non-required value with optarg_req."
   $ test_desc desc (fst (fromJust (argdesc_times desc)) == 0)
               "Bug: Querying argument which isn't optional with optarg_req."
   $ test_desc desc (snd (fromJust (argdesc_times desc)) == 1)
               "Bug: Querying argument which may occur several times optarg_req."
   $ fmap fromJust (maybe_head (argvalues args desc))


-- |
-- None of the specifed arguments may be present.
--
-- Throws an ArgError if any of the arguments are present.
args_none :: [ArgumentDescription]        -- ^ List of the arguments which must not be present.
          -> Arguments                    -- ^ Command line parse result.
          -> IO ()
args_none descs args@(Arguments argl) =
   mapM_ (\desc ->
             when (arg_times args desc /= 0) $
                argerror_ui (up1 (argname_a desc) ++ " is not allowed.\n")
                            descs
         )
         descs

-- |
-- All of the specified arguments must be present.
--
-- Throws an ArgError if any is missing.
args_all :: [ArgumentDescription]         -- ^ List of the arguments which must be present.
         -> Arguments                     -- ^ Command line parse result.
         -> IO ()
args_all descs args@(Arguments argl) =
   mapM_ (\desc ->
             when (arg_times args desc == 0) $
                argerror_ui ("Missing " ++ argname_a desc ++ "\n") descs
         )
         descs

-- |
-- Exactly one of the specified arguments must be present.
--
-- Otherwise throw an ArgError.
args_one :: [ArgumentDescription]         -- ^ List of the arguments, of which exactly one must be present.
         -> Arguments                     -- ^ Command line parse result.
         -> IO ()
args_one descs args@(Arguments argl) =
   when (occuring descs args /= 1) $
      argerror_ui ("Exactly one of the following arguments must be present.\n"
                ++ concat (intersperse ", " (map argname descs)) ++ "\n")
               descs


-- |
-- At most one of the specified arguments may be present.
--
-- Otherwise throw an ArgError.
args_at_most_one :: [ArgumentDescription] -- ^ List of the arguments, of which at most one may be present.
                 -> Arguments             -- ^ Command line parse result.
                 -> IO ()
args_at_most_one descs args@(Arguments argl) =
   when (occuring descs args > 1) $
      argerror_ui ("Only one of the following arguments may be present.\n"
                   ++ concat (intersperse ", " (map argname descs)) ++ "\n")
                   descs


-- |
-- At least one of the specified arguments must be present.
--
-- Otherwise throw an ArgError.
args_at_least_one :: [ArgumentDescription]    -- ^ List of the arguments, of which at least one must be present.
                  -> Arguments                -- ^ Command line parse result.
                  -> IO ()
args_at_least_one descs args@(Arguments argl) =
   when (occuring descs args == 0) $
      argerror_ui ("One of the following arguments must be present.\n"
                   ++ concat (intersperse ", " (map argname descs)) ++ "\n")
                   descs


-- |
-- When the specified argument is present, then none of the other arguments may be present.
--
-- Otherwise throw an ArgError.
arg_conflicts :: ArgumentDescription   -- ^ Argument which doesn't tolerate the other arguments
              -> [ArgumentDescription] -- ^ Arguments which aren't tolerated by the specified argument
              -> Arguments             -- ^ Command line parse result.
              -> IO ()
arg_conflicts desc descs args@(Arguments argl) =
   when (arg_occurs args desc && occuring descs args > 1) $
      argerror_ui ("When " ++ argname desc ++ " is present, none of the following arguments may be present.\n"
                   ++ concat (intersperse ", " (map argname descs)) ++ "\n")
                   descs


-- How many of the specified arguments do occur? Multiple occurences of the same argument count as one.
occuring :: [ArgumentDescription] -> Arguments -> Int
occuring descs args =
   sum (map (\desc -> if arg_times args desc == 0 then 0 else 1) descs)


{- | Whether the specified argument occurs in the command line.
-}
arg_occurs :: Arguments                   -- ^ Command line parse result.
           -> ArgumentDescription         -- ^ Description of the respective argument.
           -> Bool                              -- ^ Whether the specified argument occurs in the command line.
arg_occurs args desc =
   occuring [desc] args == 1


-- | /Deprecated/. This is left here for backwards compatibility. New programs should use @make_usage_info@ and/or
-- @print_usage_info@.
-- 
-- Get the usage information from the parsed arguments. The usage info
-- contains the header specified to the corresponding @getargs...@
-- function, and descriptions of the command line arguments.
--
-- Descriptions can be several lines long. Lines get wrapped at column 80.
--
-- See 'make_usage_info', 'print_usage_info', 'wrap'.
usage_info :: Arguments -> String
usage_info (Arguments (l, header)) =
   unlines (wrap 80 header) ++
   concat (intersperse "\n" (make_usage_info (map fst l) 0 10 30 80))



{- | @getargs@ as a pure function, instead of an IO action. This allows to make evaluated command line arguments
global values. This calls @getargs@ to parse the command line arguments. @GHC.IO.unsafePerformIO@ is used to take
the result out of the IO monad.

   >unsafe_getargs header descs = GHC.IO.unsafePerformIO $ getargs "" descs

   The @getargs@ action is performed on demand, when the parse result is evaluated. It may result in an 'ArgError'
   being thrown. In order to avoid this happening at unexpected times, the @main@ function should, start with the
   line @seq args (return ())@, where @args@ is the result of @unsafe_getargs@,. This will trigger any command line
   argument errors at the beginning of the program. (See section 6.2 of the Hakell Report for the definition of
   @seq@).

   The header is used only by the deprecated @usage_info@ function. If you don't
   use it, you don't need to specify a header. Just pass an empty string.

   A typical use of @unsafe_getargs@ looks like this:

>descs = [ d_myflag, ... ]
>
>d_myflag = argdesc [ ... ]
>
>args = unsafe_getargs "" descs
>myflag = arg_switch args d_myflag
>
>main = mainwrapper $ do
>   seq args (return ())
>   ...

  See 'getargs', 'unsafe_getargs_ordered'.
-}
unsafe_getargs :: String                        -- ^ Header to be used by the deprecated @usage_info@ function.
               -> [ArgumentDescription]   -- ^ The argument descriptions
               -> Arguments               -- ^ The parsed command line arguments
unsafe_getargs header descs =
   GHC.IO.unsafePerformIO $ getargs header descs


{- | @getargs_ordered@ as a pure function, instead of an IO action. This is exactly like @unsafe_getargs@, but
   using @getargs_ordered@ instead of @getargs@.

   The header is used only by the deprecated @usage_info@ function. If you don't
   use it, you don't need to specify a header. Just pass an empty string.

   The definition is:

   >unsafe_getargs_ordered = GHC.IO.unsafePerformIO $ getargs_ordered "" descs

   See 'unsafe_getargs', 'usage_info', 'make_usage_info', 'print_usage_info'.
-}
unsafe_getargs_ordered :: String                  -- ^ Header to be used by the deprecated @usage_info@ function.
                       -> [ArgumentDescription]   -- ^ The argument descriptions
                       -> Arguments               -- ^ The parsed command line arguments
unsafe_getargs_ordered header descs =
   GHC.IO.unsafePerformIO $ getargs_ordered header descs



make_usage_info1 :: [ArgumentDescription] -> String
make_usage_info1 argdescs =
   concat (intersperse "\n" (make_usage_info argdescs 0 10 30 80))



-- |
-- Generate pretty-printed information about the command line arguments. This
-- function gives you much control on how the usage information is generated.
-- @print_usage_info@ might be more like what you need.
--
-- The specified argument descriptions (as taken by the @getargs@... functions)
-- are processed in the given order. Each one is formatted as a paragraph,
-- detailing the argument. This is done according to the specified geometry.
--
-- The direct argument, in case there is one, is omitted. You should detail the
-- direct command line arguments separatly, such as in some header.
--
-- The specified maximum breadths must fit in the specified width, or an error
-- is raised. This happens, when @colsleft + colsshort + 2 + colslong + 2 + 2 >
-- width@.
--
-- See 'print_usage_info', 'getargs', 'usage_info', 'ArgumentDescription',
-- 'desc_description', 'argdesc', 'terminal_width', 'terminal_width_ioe'.

make_usage_info :: [ArgumentDescription]      -- ^ List of argument descriptions, as created by a @argdesc@
                -> Int                        -- ^ The output is indented this many columns. Probably zero.
                -> Int                        -- ^ Maximum width of the column of the short form of each argument.
                                              --   When this many aren'tneeded, less are used.
                -> Int                        -- ^ Maximum width of the column of the long form of each argument.
                                              --   When this many aren't needed, less are used.
                -> Int                        -- ^ Wrap everything at this column. Should probably be the
                                              --   terminal width.
                -> [String]                   -- ^ Pretty printed usage information, in paragraphs, which contain
                                              --   one or several lines, which are separated by newlines.

make_usage_info descs colsleft colsshort colslong width =

    if colsleft + colsshort + 2 + colslong + 2 + 2 > width
       then error $ "make_usage_info: colsleft, colsshort, and colslong arguments \
                    \are too large for the specified width argument.\n\
                    \colsleft  = " ++ show colsleft ++ "  \n\
                    \colsshort = " ++ show colsshort ++ "  \n\
                    \colslong  = " ++ show colslong ++ "  \n\
                    \width     = " ++ show width
       else
          map unlines (verbinden (zll' (filter (\d -> not (is_direct d))
                                               descs)
                                 ))

    where
          -- The argument description, wrapped to the right width.
          beschr :: ArgumentDescription -> [String]
          beschr desc = wrap (width - colsleft - gesamtbr_kurz - 2 - gesamtbr_lang - 2)
                             (fromMaybe "" (argdesc_description desc))

          -- Render an argument description.
          auff1 :: ArgumentDescription 
                -> ([String], [String], [String])
          auff1 desc = auff (kurzname desc)
                            (langname desc)
                            (beschr desc)

          -- Wir haben für eine Argumentbeschreibung die Listen von Zeilen, aus denen der kurze, und lange
          -- Argumentname besteht, sowie die Zeilen, aus denen die Argumentbeschreibung besteht.
          zus :: ([String], [String], [String]) -> [(String, String, String)]
          zus (as, bs, cs) = zip3 as bs cs

          -- Die für die Kurzform einses Arguments benötigte Zahl von Spalten
          kurzbr :: ArgumentDescription -> Int
          kurzbr desc =
             foldr max 0 (map length (kurzname desc))

          -- Die für die Langform einses Arguments benötigte Zahl von Spalten
          langbr :: ArgumentDescription -> Int
          langbr desc =
             foldr max 0 (map length (langname desc))

          -- Breite der Kurzform, über alle Argumente hinweg
          gesamtbr_kurz =
             foldr max 0 (map (\desc -> kurzbr desc) descs)

          -- Breite der Langform, über alle Argumente hinweg
          gesamtbr_lang =
             foldr max 0 (map (\desc -> langbr desc) descs)

          -- Breite der Beschreibungen
          breite_descr :: Int
          breite_descr = width - colsleft - gesamtbr_kurz - 2 - gesamtbr_lang - 2

          -- Für jedes Kommandozeilenargument die Liste der Zeilen
          zll :: [ArgumentDescription]
              -> [[(String, String, String)]]
          zll descs =
             map (zus . auff1) descs

          -- Für jedes Kommandozeilenargument die Liste der Zeilen, aufgefüllt auf einheitliche Breite
          zll' :: [ArgumentDescription]
               -> [[(String, String, String)]]
          zll' [] =
             []
          zll' descs =
             map (\l -> map (\(a,b,c) -> (fuell gesamtbr_kurz a,
                                          fuell gesamtbr_lang b,
                                          c))
                            l)
                 (zll descs)

          -- Die Tripel
          verbinden :: [[(String, String, String)]]
                    -> [[String]]
          verbinden l =
             map (\l' -> map (\(a,b,c) -> take colsleft (repeat ' ')
                                          ++ a ++ "  " ++ b ++ "  " ++ c) l')
                 l

          -- Die Kurzform des angegebenen Arguments. In Zeilen heruntergebrochen,
          -- wenn die Breite colsshort überschritten wird.
          kurzname :: ArgumentDescription -> [String]
          kurzname desc =
             wrap colsshort (argname_short desc)

          -- Die Langform des angegebenen Arguments. In Zeilen heruntergebrochen,
          -- wenn die Breite colslong überschritten wird
          langname :: ArgumentDescription -> [String]
          langname desc =
             wrap colslong (argname_long desc)

          -- Den gegebenen String um so viele Leerzeichen ergänzen, daß daraus ein String der gegebenen Länge
          -- wird. Ist er dafür zu lang, denn den unveränderten String zurückgeben.
          fuell :: Int -> String -> String
          fuell br txt =
             txt ++ take (br - length txt) (repeat ' ')


          -- Complete three lists of Strings. All three strings are made to be made up
          -- of the same number of entries. Missing entries at the end are filled up with
          -- empty strings.
          auff :: [String] -> [String] -> [String] -> ([String], [String], [String])
          auff a b c =
             (reverse x, reverse y, reverse z)

             where
                (x,y,z) = auff' a b c [] [] []

                auff' :: [String] -> [String] -> [String]
                      -> [String] -> [String] -> [String]
                      -> ([String], [String], [String])

                auff' [] [] [] a1 b1 c1 =
                   (a1, b1, c1)

                auff' a b c a1 b1 c1 =
                   auff' (if null a then [] else tail a)
                         (if null b then [] else tail b)
                         (if null c then [] else tail c)
                         ((if null a then "" else head a) : a1)
                         ((if null b then "" else head b) : b1)
                         ((if null c then "" else head c) : c1)




-- |
-- Print the usage information (about the command line arguments), for the
-- specified header and arguments to the specified handle. When the handle is
-- connected to a terminal, the terminal\'s width (in columns) is used to format
-- the output, such that it fits the terminal. Both the header and the argument
-- descriptions are adapted to the width of the terminal (by using @wrap@).
--
-- When the handle does not connected to a terminal, 80 columns are used. This
-- may happen to @stdout@ or @stderr@, for instance, when the program is in a
-- pipe, or the output has been redirected to a file.
--
-- When the terminal is too narrow for useful output, then instead of the usage
-- information, a short message (@"Terminal too narrow"@) is printed. This
-- applies to terminals with a width of less than 12.
--
-- You should specify one long line for each paragraph in the header and the
-- argument descriptions, and let print_usage_info do the wrapping. When you
-- have several paragraphs, separate them by a double @\\n\\n@. This also applies
-- for an empty line, which should be printed after the actual header.
--
-- The arguments are printed in the order, in which they occur in the argument
-- description list.
--
-- This function is a front end to @terminal_width@ and @make_usage_info@.
--
-- See 'argdesc', 'desc_description', 'terminal_width', 'make_usage_info', 'usage_info', 'wrap'.
print_usage_info :: Handle                      -- ^ To which handle to print the
                                                --   usage info.
                 -> String                      -- ^ The header to print first.
                                                --   Can be empty.
                 -> [ArgumentDescription]       -- ^ The argument description of
                                                --   the arguments, which should be documented.
                 -> IO ()
print_usage_info h header descs = do

   -- Determine the width to use
   mw <- terminal_width h
   let w = case mw of
              Just w  -> w
              Nothing -> 80

   {-
   if w < 12
      then ioError (mkIOError userErrorType "The terminal width is too small (< 12) for printing \
                                            \of the usage information. See print_usage_info." (Just h) Nothing)
      else
   -}

   if w < 12
      then hPutStr h "Terminal too narrow"

      else do -- Wrap and print the header
              hPutStr h (unlines (wrap w header))

              -- Print the argument descriptions.
              mapM_ (hPutStr h)
                    (make_usage_info descs
                                     0
                                     (w `div` 5)
                                     (w `div` 3)
                                     w)


-- |
-- Break down a text to lines, such that each one has the specified
-- maximum width.
--
-- Newline characters in the input text are respected. They terminate the line,
-- without it being filled up to the width.
--
-- The text is wrapped at space characters. Words remain intact, except when
-- they are too long for one line.
wrap :: Int             -- ^ Maximum width for the lines of the text, which is to be broken down
     -> String          -- ^ Text to break down
     -> [String]        -- ^ The broken down text in columns
wrap breite [] = []
wrap breite txt =
   [ zl | txtzl <- lines txt,
          zl <- wrap' breite txtzl
   ]
   where
      wrap' :: Int -> String -> [String]
      wrap' breite [] = [""]
      wrap' breite txt =
         wrap'' breite (dropWhile isSpace txt)

      wrap'' :: Int -> String -> [String]
      wrap'' breite txt =
         if length txt <= breite
            then [txt]
            else
                 if null txt_anf
                    then -- Zu breit für eine Zeile
                         txt_br : wrap' breite txt_rest
                    else txt_anf : wrap' breite rest

         where
            (txt_br, txt_rest) =
               splitAt breite txt

            (txt_anf, txt_anf_rest) =
               letzter_teil txt_br

            rest = txt_anf_rest ++ txt_rest

            -- Letztes Wort von zl abspalten. Liefert
            -- ( Anfang von zl, Letztes Wort )
            letzter_teil zl =
               let zl'              = reverse zl
                   (wort, zl'')     = span (/= ' ') zl'
                   zl''1            = dropWhile (== ' ') zl''
                   zl'''            = reverse zl''1
                   wort'            = reverse wort
               in (zl''', wort')





