{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

{- |
  The default representation of any type is the string and the @show@ function, which returns a string for each element
  of an instance of the @Show@ class. But sometimes, it is more intuitive, if a type is represented by a more
  three-dimensional layout. For that matter, we not only have a @show@ function for many types to come, but also a
  @textFrame@ converter, where a @TextFrame@ is basically defined as a list of strings of equal length.
  Similar to the @Show@ type class, we also define a @Display@ type class.
-}

module TextDisplay (

  -- * Text frames

  TextFrame,
  -- | A TextFrame is a list of strings. But for a /correct/ TextFrame, there are more constraints that have to hold:
  -- 1. The strings must all be of equal length.
  -- 2. There must be no white space characters in a string, other than the space character ' '.

  isNonSpaceWhite,
  -- | True if the character is any white space character, except the space character itself.

  findTextFrameError,
  -- | A TextFrame is /correct/ iff all its strings are of equal length and (isNonSpaceWhite ch) is false for all characters ch.

  correctTextFrame,
  -- | Turns the argument string list into a correct version by adding spaces. But returns an error in case any string contains a character ch with (isNonSpaceWhite ch).
  -- IMPROVE the String instance of Display: new line characters should create new lines in the text frame and what about tabs etc? !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  width,
  -- | The width of a TextFrame [str1,...,strN] is the maximal length of the strings str1,...,strN.
  -- In particular,
  --
  -- > (width []) == 0           (width [""]) == 0
  --

  height,
  -- | The height of a TextFrame [str1,...,strN] is N.

  printTextFrame,
  -- | prints its TextFrame argument.

  textFrameBox,
  -- | surrounds the text frame with a solid line

  textFrameBracket,
  -- | surrounds the text frame with a square bracket

  defaultTextFrame,
  -- | @(defaultTextFrame x) == [show x]@

  -- * The @Display@ type class
  Display(..),
  -- | @textFrame@ converts into a @TextFrame@.
  -- @display@ prints the text frame (actually, @display = printTextFrame . textFrame@, i.e. to define an instance of @Display@,
  -- Actually, when an instance of @Display@ is defined, only @textFrame@ needs to be specified.


  -- * Text frame tables
  TextFrameTable,
  -- | A @TextFrameTable@ is a list of rows, where each cell is a @TextFrame@. For example,
  --
  -- > [[["aaa","aaa"],["b","b","b"]],
  -- >  [["cccccccc","cccccccc"],["ddddddddd","ddddddddd","ddddddddd"],["eeeee","eeeee","eeeee","eeeee"]],
  -- >  [["ff","ff"],[],["ggg","ggg","ggg","ggg"]]]
  --
  -- or, more intuitively in table layout
  --
  -- >   "aaa"      "b"
  -- >   "aaa"      "b"
  -- >              "b"
  -- >
  -- >   "cccccccc" "ddddddddd" "eeeee"
  -- >   "cccccccc" "ddddddddd" "eeeee"
  -- >              "ddddddddd" "eeeee"
  -- >                          "eeeee"
  -- >
  -- >   "ff"                   "ggg"
  -- >   "ff"                   "ggg"
  -- >                          "ggg"
  -- >                          "ggg"
  --
  columnWidthList,
  rowHeightList,
  -- | Column widths and row heights for the previous example text frame table @tft@ are given by
  --
  -- > (columnWidthList tft) == [8,9,5]
  -- > (rowHeightList tft)   == [3,4,4]
  --
  correctTextFrameTable,
  -- | A @TextFrameTable@ is said to be /correct/, if
  -- (1) each row has the same amount of cells,
  -- (2) there is no column of zero width,
  -- (3) there is no row of zero height.
  -- With @correctTextFrameTable@ we remove these flaws.
  --
  bottomAlign, topAlign, centerAlign,
  leftAlign, rightAlign, middleAlign,
  -- | A @TextFrameTable@ is said to be /normal/, if it is correct and each of its @TextFrame@ cells has the width of its according column
  -- and the height of its row.
  -- To convert a text frame table into a normal one, we need to perform two steps (in arbitrary order):
  --
  --   * A row normalization, which makes all text frame cells in one row of equal height. We use three modes to achieve that:
  --     @bottomAlign@, @topAlign@ and @centerAlign@.
  --
  --   * A column normalization, which makes all text frame cells in one column of equal width. Again, we have three functions to do that:
  --     @leftAlign@, @rightAlign@, and @middleAlign@.
  --
  -- For example, given the correct text frame table @tft@, we obtain (we use @_@ for space characters):
  --
  -- >   tft =                               leftAlign tft =                      middleAlign (leftAlign tft) =
  -- >
  -- >   "aaa"      "b"         ""           "aaa ____" "b________" "_____"       "aaa_____" "b________" "_____"
  -- >   "aaa"      "b"                      "aaa_____" "b________"               "aaa_____" "b________" "_____"
  -- >              "b"                                 "b________"               "________" "b________" "_____"
  -- >
  -- >   "cccccccc" "ddddddddd" "eeeee"      "cccccccc" "ddddddddd" "eeeee"       "________" "ddddddddd" "eeeee"
  -- >   "cccccccc" "ddddddddd" "eeeee"      "cccccccc" "ddddddddd" "eeeee"       "cccccccc" "ddddddddd" "eeeee"
  -- >              "ddddddddd" "eeeee"                 "ddddddddd" "eeeee"       "cccccccc" "ddddddddd" "eeeee"
  -- >                          "eeeee"                             "eeeee"       "________" "_________" "eeeee"
  -- >
  -- >   "ff"                   "ggg"        "ff______" "_________" "ggg__"       "________" "_________" "ggg__"
  -- >   "ff"                   "ggg"        "ff______"             "ggg__"       "ff______" "_________" "ggg__"
  -- >                          "ggg"                               "ggg__"       "ff______" "_________" "ggg__"
  -- >                          "ggg"                               "ggg__"       "________" "_________" "ggg__"
  --
  --
  normalTextFrameTable,
  -- | The default way to convert any text frame table into a normal one is defined by the @normalTextFrameTable@ function,
  -- which is defined by
  --
  -- > normalTextFrameTable = middleAlign . centerAlign . correctTextFrameTable
  --
  plainMerge, gridMerge,
  -- | There is a @plainMerge@ and a @gridMerge@ to turn a normal text frame table into a single text frame. For the example,
  --
  -- >   tft =                              plainMerge tft =               gridMerge tft =
  -- >
  -- >   "___aaa__" "____b____" "_____"     "___aaa______b_________"       "+----------+-----------+-------+"
  -- >   "___aaa__" "____b____" "_____"     "___aaa______b_________"       "| ___aaa__ | ____b____ | _____ |"
  -- >   "________" "____b____" "_____"     "____________b_________"       "| ___aaa__ | ____b____ | _____ |"
  -- >                                      "________dddddddddeeeee"       "| ________ | ____b____ | _____ |"
  -- >   "________" "ddddddddd" "eeeee"     "ccccccccdddddddddeeeee"       "+----------+-----------+-------+"
  -- >   "cccccccc" "ddddddddd" "eeeee"     "ccccccccdddddddddeeeee"       "| ________ | ddddddddd | eeeee |"
  -- >   "cccccccc" "ddddddddd" "eeeee"     "_________________eeeee"       "| cccccccc | ddddddddd | eeeee |"
  -- >   "________" "_________" "eeeee"     "__________________ggg_"       "| cccccccc | ddddddddd | eeeee |"
  -- >                                      "___ff_____________ggg_"       "| ________ | _________ | eeeee |"
  -- >   "________" "_________" "_ggg_"     "___ff_____________ggg_"       "+----------+-----------+-------+"
  -- >   "___ff___" "_________" "_ggg_"     "__________________ggg_"       "| ________ | _________ | _ggg_ |"
  -- >   "___ff___" "_________" "_ggg_"                                    "| ___ff___ | _________ | _ggg_ |"
  -- >   "________" "_________" "_ggg_"                                    "| ___ff___ | _________ | _ggg_ |"
  -- >                                                                     "| ________ | _________ | _ggg_ |"
  -- >                                                                     "+----------+-----------+-------+"
  --

) where ---------------------------------------------------------------------------------------------------------------

-- import

  import qualified Char as Ch
  import qualified List as L

-- basic definitions and basic functions

  type TextFrame = [String]

  isNonSpaceWhite :: Char -> Bool
  isNonSpaceWhite ch = (Ch.isSpace ch) && (ch /= ' ')

  findTextFrameError :: [String] -> Maybe String
  findTextFrameError [] = Nothing
  findTextFrameError (str:strL) = iter(length str, strL)
    where iter(n,[]) = Nothing
          iter(n,str:strL) = if any isNonSpaceWhite str
                             then Just ("String contains illegat white space characters: \n" ++ str)
                             else let n' = length str
                                  in if n' == n
                                     then iter(n,strL)
                                     else Just (concat ["Contains strings of different length (e.g. ",
                                                        show n, " and ", show n', ")."])

  correctTextFrame :: [String] -> TextFrame
  correctTextFrame tf =
    if or (map (\ str -> any isNonSpaceWhite str) tf)
    then error "Illegal white space characters."
    else let w = width tf
         in (map (\ str -> str ++ (replicate (w - (length str)) ' ')) tf)

  width :: TextFrame -> Int
  width [] = 0
  width strL = maximum (map length strL)

  height :: TextFrame -> Int
  height = length

  printTextFrame :: TextFrame -> IO ()
  printTextFrame = putStr . unlines

  textFrameBox :: TextFrame -> TextFrame
  textFrameBox tf = [rule] ++ (map (\ str -> ("| " ++ str ++ " |")) tf) ++ [rule]
    where w = width tf
          rule = "+" ++ (replicate (w + 2) '-') ++ "+"

  textFrameBracket :: TextFrame -> TextFrame
  textFrameBracket [] = ["[]"]
  textFrameBracket [str] = [ "[" ++ str ++ "]" ]
  textFrameBracket strL = line ++ (map (\ str -> "| " ++ str ++ " |") strL) ++ line
    where line = [ "+-" ++ (replicate (width strL) ' ') ++ "-+" ]

  defaultTextFrame :: Show a => a -> TextFrame
  defaultTextFrame x = [show x]

-- the Display type class

  class Display a where
    textFrame :: a -> TextFrame
    display :: a -> IO ()
    display = printTextFrame . textFrame

  instance Display Bool where
    textFrame b = if b then ["1"] else ["0"]

  instance Display Int where
    textFrame = defaultTextFrame

  instance Display Integer where
    textFrame = defaultTextFrame

  instance Display Float where
    textFrame = defaultTextFrame

  instance Display Double where
    textFrame = defaultTextFrame

  instance Display Char where
    textFrame ch = [[ch]]

  instance Display String where
    textFrame str = [str]

  instance Display () where
    textFrame () = [""]

-- text frame tables

  type TextFrameTable = [[TextFrame]]

  columnWidthList :: TextFrameTable -> [Int]
  columnWidthList tft = iter [] tft
    where listMax [] mL = mL
          listMax nL [] = nL
          listMax (n:nL) (m:mL) = (max n m) : (listMax nL mL)
          iter nL [] = nL
          iter nL (row:tft) = iter (listMax nL (map width row)) tft

  rowHeightList :: TextFrameTable -> [Int]
  rowHeightList tft = map (\ row -> (maximum (0 : (map (\ cell -> (height cell)) row)))) tft

  correctTextFrameTable :: TextFrameTable -> TextFrameTable
  correctTextFrameTable tft = tft''''
    where colWidthL = columnWidthList tft
          rowHeightL = rowHeightList tft
          -- 1. remove empty rows
          tft' = filter (not . null) tft
          -- 2. fill up the rows to equal length
          colNumber = length colWidthL
          dummyTextFrame = [""]  :: TextFrame
          fillRow row = row ++ (replicate (colNumber - (length row)) dummyTextFrame)
          tft'' = map fillRow tft'  :: TextFrameTable
          -- 3. remove zero width columns
          remove [] [] = []
          remove (n:nL) (cell:row) = if n == 0
                                     then remove nL row
                                     else cell : (remove nL row)
          remove _ _ = error "correctTextFrameTable -- unexpected error!"
          tft''' = map (remove colWidthL) tft''
          -- 4. remove empty rows again
          tft'''' = filter (not . null) tft'''

  bottomAlign :: TextFrameTable -> TextFrameTable
  bottomAlign tft = allRows tft (rowHeightList tft)
    where flushBottom tf h = (replicate (h - (height tf)) (replicate (width tf) ' ')) ++ tf
          oneRow row h = map (\ cell -> (flushBottom cell h)) row
          allRows tft rowHeightList = map (\ (row,h) -> oneRow row h) (zip tft rowHeightList)

  topAlign :: TextFrameTable -> TextFrameTable
  topAlign tft = allRows tft (rowHeightList tft)
    where flushTop tf h = tf ++ (replicate (h - (height tf)) (replicate (width tf) ' '))
          oneRow row h = map (\ cell -> (flushTop cell h)) row
          allRows tft rowHeightList = map (\ (row,h) -> oneRow row h) (zip tft rowHeightList)

  centerAlign :: TextFrameTable -> TextFrameTable
  centerAlign tft = allRows tft (rowHeightList tft)
    where center tf h = let emptyRow = (replicate (width tf) ' ')
                            h' = height tf
                            topBlock = replicate ((h - h') `div` 2) emptyRow
                            botBlock = replicate (((h - h') `div` 2) + ((h - h') `mod` 2)) emptyRow
                        in topBlock ++ tf ++ botBlock
          oneRow row h = map (\ cell -> (center cell h)) row
          allRows tft rowHeightList = map (\ (row,h) -> oneRow row h) (zip tft rowHeightList)

  leftAlign :: TextFrameTable -> TextFrameTable
  leftAlign tft = map rowAlign tft
    where colWidthL = columnWidthList tft
          rowAlign row = map (\ (cell,w) -> (flushLeft cell w)) (zip row colWidthL)
          flushLeft tf n = map (\ str -> (str ++ (replicate (n - (length str)) ' '))) tf

  rightAlign :: TextFrameTable -> TextFrameTable
  rightAlign tft = map rowAlign tft
    where colWidthL = columnWidthList tft
          rowAlign row = map (\ (cell,w) -> (flushRight cell w)) (zip row colWidthL)
          flushRight tf n = map (\ str -> ((replicate (n - (length str)) ' ') ++ str)) tf

  middleAlign :: TextFrameTable -> TextFrameTable
  middleAlign tft = map rowAlign tft
    where colWidthL = columnWidthList tft
          rowAlign row = map (\ (cell,w) -> (centralize cell w)) (zip row colWidthL)
          centralize tf n = map (\ str -> (center str (length str) n)) tf
          center str l n = let d = (n - l) `div` 2
                               m = (n - l) `mod` 2
                           in (replicate d ' ') ++ str ++ (replicate (d + m) ' ')

  normalTextFrameTable :: TextFrameTable -> TextFrameTable
  normalTextFrameTable = middleAlign . centerAlign . correctTextFrameTable

  plainMerge :: TextFrameTable -> TextFrame
  plainMerge tft = concat (map mergeTextFrames tft)
    where mergeTextFrames tfL = map concat (L.transpose tfL)

  gridMerge :: TextFrameTable -> TextFrame
  gridMerge tft = tf
    where oneLine strL = "| " ++ (concat (L.intersperse " | " strL)) ++ " |"
          oneFrameRow row = map oneLine (L.transpose row)
          lineRow = "+-" ++ (concat (L.intersperse "-+-" (map (\ n -> (replicate n '-')) (columnWidthList tft)))) ++ "-+"
          tf = [lineRow] ++ (concat (L.intersperse [lineRow] (map oneFrameRow tft))) ++ [lineRow]



