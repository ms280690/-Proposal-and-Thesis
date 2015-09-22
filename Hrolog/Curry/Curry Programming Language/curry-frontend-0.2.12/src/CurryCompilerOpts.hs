-- -----------------------------------------------------------------------------
-- |
-- CurryCompilerOpts - Defines data structures containing options for
--                     compiling Curry programs (see module "CurryCompiler")
--
-- September 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
-- March 2007, extensions by Sebastian Fischer (sebf@informatik.uni-kiel.de)
--
-- -----------------------------------------------------------------------------

module CurryCompilerOpts where

import System.Console.GetOpt


-- | Data type for recording compiler options
data Options
  = Options
  { force :: Bool             -- ^ force compilation
  , html :: Bool              -- ^ generate Html code
  , importPaths :: [FilePath] -- ^ directories for searching imports
  , output :: Maybe FilePath  -- ^ name of output file
  , noInterface :: Bool       -- ^ do not create an interface file
  , noVerb :: Bool            -- ^ verbosity on/off
  , noWarn :: Bool            -- ^ warnings on/off
  , noOverlapWarn :: Bool     -- ^ "overlap" warnings on/off
  , flat :: Bool              -- ^ generate FlatCurry code
  , extendedFlat :: Bool      -- ^ generate FlatCurry code with extensions
  , flatXml :: Bool           -- ^ generate flat XML code
  , abstract :: Bool          -- ^ generate typed AbstracCurry code
  , untypedAbstract :: Bool   -- ^ generate untyped AbstractCurry code
  , parseOnly :: Bool         -- ^ generate source representation
  , withExtensions :: Bool    -- ^ enable extended functionalities
  , dump :: [Dump]            -- ^ dumps
  , writeToSubdir :: Bool     -- ^ should the output be written to the subdir?
  } deriving Show


-- | Default compiler options
defaultOpts = Options
  { force           = False
  , html            = False
  , importPaths     = []
  , output          = Nothing
  , noInterface     = False
  , noVerb          = False
  , noWarn          = False
  , noOverlapWarn   = False
  , extendedFlat    = False
  , flat            = False
  , flatXml         = False
  , abstract        = False
  , untypedAbstract = False
  , parseOnly       = False
  , withExtensions  = False
  , dump            = []
  , writeToSubdir   = True
  }


-- | Data type for representing all available options (needed to read and parse
--   the options from the command line; see module 'GetOpt')
data Option
  = Help | Force | Html
  | ImportPath FilePath | Output FilePath
  | NoInterface | NoVerb | NoWarn | NoOverlapWarn
  | FlatXML | Flat | ExtFlat | Abstract | UntypedAbstract | ParseOnly
  | WithExtensions
  | Dump [Dump]
  | WriteToSubdir
  deriving Eq


-- | All available compiler options
options =
  [ Option "f"  ["force"] (NoArg Force)
              "force compilation of dependent files"
  , Option ""   ["html"] (NoArg Html)
              "generate html code"
  , Option "i"  ["import-dir"] (ReqArg ImportPath "DIR")
              "search for imports in DIR"
  , Option "o"  ["output"] (ReqArg Output "FILE")
              "write code to FILE"
  , Option ""   ["no-intf"] (NoArg NoInterface)
              "do not create an interface file"
  , Option ""   ["no-verb"] (NoArg NoVerb)
          "do not print compiler messages"
  , Option ""   ["no-warn"] (NoArg NoWarn)
          "do not print warnings"
  , Option ""   ["no-overlap-warn"] (NoArg NoOverlapWarn)
          "do not print warnings for overlapping rules"
  , Option ""   ["flat"] (NoArg Flat)
              "generate FlatCurry code"
  , Option ""   ["extended-flat"] (NoArg ExtFlat)
              "generate FlatCurry code with source references"
  , Option ""   ["xml"] (NoArg FlatXML)
              "generate flat xml code"
  , Option ""   ["acy"] (NoArg Abstract)
              "generate (type infered) AbstractCurry code"
  , Option ""   ["uacy"] (NoArg UntypedAbstract)
              "generate untyped AbstractCurry code"
  , Option ""   ["parse-only"] (NoArg ParseOnly)
              "generate source representation"
  , Option "e"  ["extended"] (NoArg WithExtensions)
              "enable extended Curry functionalities"
  , Option ""   ["dump-all"] (NoArg (Dump [minBound..maxBound]))
              "dump everything"
  , Option ""   ["dump-renamed"] (NoArg (Dump [DumpRenamed]))
              "dump source code after renaming"
  , Option ""   ["dump-types"] (NoArg (Dump [DumpTypes]))
              "dump types after type-checking"
  , Option ""   ["dump-desugared"] (NoArg (Dump [DumpDesugared]))
              "dump source code after desugaring"
  , Option ""   ["dump-simplified"] (NoArg (Dump [DumpSimplified]))
              "dump source code after simplification"
  , Option ""   ["dump-lifted"] (NoArg (Dump [DumpLifted]))
              "dump source code after lambda-lifting"
  , Option ""   ["dump-il"] (NoArg (Dump [DumpIL]))
              "dump intermediate language before lifting"
  , Option ""   ["dump-case"] (NoArg (Dump [DumpCase]))
              "dump intermediate language after case simplification"
  , Option "?h" ["help"] (NoArg Help)
              "display this help and exit"
  , Option ""   ["no-hidden-subdir"] (NoArg WriteToSubdir)
              "write all output to hidden .curry subdirectory"
  ]


-- | Marks an 'Option' as selected in the 'Options' record
selectOption :: Option -> Options -> Options
selectOption Force opts           = opts { force = True }
selectOption (ImportPath dir) opts
   = opts { importPaths = dir:(importPaths opts) }
selectOption (Output file) opts   = opts { output = Just file }
selectOption NoInterface opts     = opts { noInterface = True }
selectOption NoVerb opts          = opts { noVerb = True }
selectOption NoWarn opts          = opts { noWarn = True }
selectOption NoOverlapWarn opts   = opts { noOverlapWarn = True }
selectOption Flat opts            = opts { flat = True }
selectOption ExtFlat opts         = opts { extendedFlat = True }
selectOption Html opts            = opts { html = True }
selectOption FlatXML opts         = opts { flatXml = True }
selectOption Abstract opts        = opts { abstract = True }
selectOption UntypedAbstract opts = opts { untypedAbstract = True }
selectOption ParseOnly opts       = opts { parseOnly = True }
selectOption WithExtensions opts  = opts { withExtensions = True }
selectOption (Dump ds) opts       = opts { dump = ds ++ dump opts }
selectOption WriteToSubdir opts   = opts { writeToSubdir = False }


-- | Data type for representing code dumps
--   TODO: dump FlatCurry code, dump AbstractCurry code, dump after 'case'
--   expansion
data Dump
  = DumpRenamed      -- ^ dump source after renaming
  | DumpTypes        -- ^ dump types after typechecking
  | DumpDesugared    -- ^ dump source after desugaring
  | DumpSimplified   -- ^ dump source after simplification
  | DumpLifted       -- ^ dump source after lambda-lifting
  | DumpIL           -- ^ dump IL code after translation
  | DumpCase         -- ^ dump IL code after case elimination
    deriving (Eq,Bounded,Enum,Show)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------