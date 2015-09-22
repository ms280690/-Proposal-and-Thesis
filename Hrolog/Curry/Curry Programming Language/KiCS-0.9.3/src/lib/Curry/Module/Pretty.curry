--- This library provides pretty printing combinators.
--- The interface is that of 
--- <a href="http://www.cs.uu.nl/~daan/download/pprint/pprint.html">Daan Leijen's library</a> 
--- (<code>fill</code>, <code>fillBreak</code> and <code>indent</code>
--- are missing) with a
--- <a href="http://www.cs.kent.ac.uk/pubs/2006/2381/index.html">linear-time, bounded implementation</a> by Olaf Chitil.
---
--- @author Sebastian Fischer
--- @version October 2006
---
module Pretty (

  -- pretty printer and document type
  pretty, Doc, 

  -- basic document combinators
  empty, text, linesep, line, linebreak, group, softline, softbreak,

  -- alignment combinators
  nest, hang, align, --indent??,

  -- composition combinators
  combine, (<>), (<+>), (<$>), (</>), (<$$>), (<//>),

  -- list combinators
  compose, hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, 
  punctuate, encloseSep, hEncloseSep, fillEncloseSep, list, tupled, semiBraces,

  -- bracketing combinators
  enclose, squotes, dquotes, bquotes, parens, angles, braces, brackets,

  -- primitve type documents
  char, string, int, float,

  -- character documents
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  squote, dquote, semi, colon, comma, space, dot, backslash, equals

  ) where

import Dequeue

infixl 1 <>, <+>, <$>, </>, <$$>, <//>

data Doc = Doc (Tokens -> Tokens)

deDoc (Doc d) = d

empty :: Doc
empty = text ""

text :: String -> Doc
text s = Doc (Text s)

linesep :: String -> Doc
linesep = Doc . Line

line, linebreak, softline, softbreak :: Doc
line = linesep " "
linebreak = linesep ""
softline = group line
softbreak = group linebreak

group :: Doc -> Doc
group d = Doc (Open . deDoc d . Close)

nest, hang :: Int -> Doc -> Doc
nest i d = Doc (OpenNest (\ms@(m:_) _ _ -> (m+i):ms) . deDoc d . CloseNest)
hang i d = Doc (OpenNest (\ms r w -> (w-r+i):ms) . deDoc d . CloseNest)

align :: Doc -> Doc
align = hang 0

combine :: Doc -> Doc -> Doc -> Doc
combine s d1 d2 = enclose d1 d2 s

(<>), (<+>), (<$>), (</>), (<$$>), (<//>) :: Doc -> Doc -> Doc
d1 <> d2 = Doc (deDoc d1 . deDoc d2)
(<+>) = combine space
(<$>) = combine line
(</>) = combine softline
(<$$>) = combine linebreak
(<//>) = combine softbreak

compose :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
--compose op = foldr op empty
compose _ [] = empty
compose op ds@(_:_) = foldr1 op ds -- no seperator at the end

hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat :: [Doc] -> Doc
hsep = compose (<+>)
vsep = compose (<$>)
fillSep = compose (</>)
sep = group . vsep
hcat = compose (<>)
vcat = compose (<$$>)
fillCat = compose (<//>)
cat = group . vcat

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate d ds@(_:_) = go ds
 where
  go [x] = [x]
  go (x:xs@(_:_)) = (x <> d) : go xs

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep l r _ [] = l <> r
encloseSep l r s (d:ds) = align (enclose l r (cat (d:map (s<>) ds)))

hEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hEncloseSep l r _ [] = l <> r
hEncloseSep l r s (d:ds) = align (enclose l r (hcat (d:map (s<>) ds)))

fillEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
fillEncloseSep l r _ [] = l <> r
fillEncloseSep l r s (d:ds)
  = align (enclose l r (hcat (d:withSoftBreaks (map (s<>) ds))))
 where
  withSoftBreaks [] = []
  withSoftBreaks [x] = [group (linebreak <> x)]
  withSoftBreaks (x:xs@(_:_))
    = (group (linebreak <> (group (x <> linebreak))) : withSoftBreaks xs)

list, tupled, semiBraces :: [Doc] -> Doc
list = fillEncloseSep lbracket rbracket comma
tupled = fillEncloseSep lparen rparen comma
semiBraces = fillEncloseSep lbrace rbrace semi

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

squotes, dquotes, parens, angles, braces, brackets :: Doc -> Doc
squotes = enclose squote squote
dquotes = enclose dquote dquote
bquotes = enclose bquote bquote
parens = enclose lparen rparen
angles = enclose langle rangle
braces = enclose lbrace rbrace
brackets = enclose lbracket rbracket

char :: Char -> Doc
char c = text [c]

string :: String -> Doc
string = hcat . map (\c -> if elem c ['\n','\r'] then line else char c)

int :: Int -> Doc
int n = text (show n)

float :: Float -> Doc
float x = text (show x)

lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  squote, dquote, bquote, semi, colon, comma, space, dot, backslash, 
  equals :: Doc
lparen = char '('
rparen = char ')'
langle = char '<'
rangle = char '>'
lbrace = char '{'
rbrace = char '}'
lbracket = char '['
rbracket = char ']'
squote = char '\''
dquote = char '\"'
bquote = char '`'
semi = char ';'
colon = char ':'
comma = char ','
space = char ' '
dot = char '.'
backslash = char '\\'
equals = char '='


type Layout = String
type Horizontal = Bool
type Remaining = Int
type Width = Int
type Position = Int
type StartPosition = Int
type EndPosition = Int
type Out = Remaining -> Margins -> String
type OutGroupPrefix = Horizontal -> Out -> Out
type Margins = [Int]

data Tokens = Text String Tokens
            | Line String Tokens
            | Open Tokens
            | Close Tokens
            | Empty
            | OpenNest (Margins -> Remaining -> Width -> Margins) Tokens
            | CloseNest Tokens

normalise :: Tokens -> Tokens
normalise = go id
  where
  go co Empty = co Empty
    -- there should be no deferred opening brackets
  go co (Open ts) = go (co . open) ts
  go co (Close ts) = go (co . Close) ts
  go co (Line s ts) = co . Line s . go id $ ts
  go co (Text s ts) = Text s (go co ts)
  go co (OpenNest f ts) = OpenNest f (go co ts)
  go co (CloseNest ts) = CloseNest (go co ts)

  open t = case t of Close ts -> ts; _ -> Open t

doc2Tokens (Doc d) = normalise (d Empty)


pretty :: Width -> Doc -> String
pretty w d = noGroup (doc2Tokens d) w 1 w [0]


length = Prelude.length . filter (not . (`elem` ([5,6,7]++[16..31])) . ord)

noGroup :: Tokens -> Width -> Position -> Out
noGroup Empty _ _ _ _ = ""
noGroup (Text t ts) w p r ms = t ++ noGroup ts w (p+l) (r-l) ms
  where
  l = length t
noGroup (Line _ ts) w p _ ms@(m:_) = 
  '\n' : replicate m ' ' ++ noGroup ts w (p+1) (w-m) ms
noGroup (Open ts) w p r ms = oneGroup ts w p (p+r) (\_ c -> c) r ms
noGroup (Close ts) w p r ms = noGroup ts w p r ms -- may have been pruned
noGroup (OpenNest f ts) w p r ms = noGroup ts w p r (f ms r w)
noGroup (CloseNest ts) w p r ms = noGroup ts w p r (tail ms)

oneGroup :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix -> Out
oneGroup (Text t ts) w p e outGrpPre = 
  pruneOne ts w (p+l) e (\h c -> outGrpPre h (outText c))
  where
  l = length t
  outText c r ms = t ++ c (r-l) ms
oneGroup (Line s ts) w p e outGrpPre = 
  pruneOne ts w (p + lens) e (\h c -> outGrpPre h (outLine h c))
  where
  lens = length s
  outLine h c r ms@(m:_) = 
    if h then s ++ c (r-lens) ms else '\n' : replicate m ' ' ++ c (w-m) ms
oneGroup (Open ts) w p e outGrpPre =
  multiGroup ts w p e outGrpPre Dequeue.empty p (\_ c -> c)
oneGroup (Close ts) w p e outGrpPre = outGrpPre (p<=e) (noGroup ts w p) 
oneGroup (OpenNest f ts) w p e outGrpPre =
  oneGroup ts w p e (\h c -> outGrpPre h (\r ms -> c r (f ms r w)))
oneGroup (CloseNest ts) w p e outGrpPre =
  oneGroup ts w p e (\h c -> outGrpPre h (\r ms -> c r (tail ms)))

multiGroup :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix 
              -> Queue (StartPosition,OutGroupPrefix) 
              -> StartPosition -> OutGroupPrefix -> Out
multiGroup (Text t ts) w p e outGrpPreOuter qs s outGrpPreInner =
  pruneMulti ts w (p+l) e outGrpPreOuter qs s 
    (\h c -> outGrpPreInner h (outText c))
  where
  l = length t
  outText c r ms = t ++ c (r-l) ms
multiGroup (Line s ts) w p e outGrpPreOuter qs si outGrpPreInner =
  pruneMulti ts w (p + lens) e outGrpPreOuter qs si 
    (\h c -> outGrpPreInner h (outLine h c))
  where
  lens = length s
  outLine h c r ms@(m:_) = 
    if h then s ++ c (r-lens) ms else '\n': replicate m ' ' ++ c (w-m) ms
multiGroup (Open ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter (cons (si,outGrpPreInner) qs) p (\_ c -> c)
multiGroup (Close ts) w p e outGrpPreOuter qs si outGrpPreInner =
  case matchHead qs of
    Nothing -> oneGroup ts w p e 
                 (\h c -> outGrpPreOuter h 
                            (\ri -> outGrpPreInner (p<=si+ri) c ri))
    Just ((s,outGrpPre),qs') ->
      multiGroup ts w p e outGrpPreOuter qs' s
        (\h c -> outGrpPre h (\ri -> outGrpPreInner (p<=si+ri) c ri))
multiGroup (OpenNest f ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si 
    (\h c -> outGrpPreInner h (\r ms -> c r (f ms r w)))
multiGroup (CloseNest ts) w p e outGrpPreOuter qs si outGrpPreInner =
  multiGroup ts w p e outGrpPreOuter qs si
    (\h c -> outGrpPreInner h (\r ms -> c r (tail ms)))


pruneOne :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix -> Out
pruneOne ts w p e outGrpPre = 
  if p <= e then oneGroup ts w p e outGrpPre 
            else outGrpPre False (noGroup ts w p)

pruneMulti :: Tokens -> Width -> Position -> EndPosition -> OutGroupPrefix 
              -> Queue (StartPosition,OutGroupPrefix) 
              -> StartPosition -> OutGroupPrefix -> Out
pruneMulti ts w p e outGrpPreOuter qs si outGrpPreInner =
  if p <= e then multiGroup ts w p e outGrpPreOuter qs si outGrpPreInner
            else outGrpPreOuter False (\r ->
                   (case matchLast qs of
                      Nothing -> pruneOne ts w p (si+r) outGrpPreInner
                      Just ((s,outGrpPre),qs') ->
                        pruneMulti ts w p (s+r) outGrpPre qs' si outGrpPreInner)
                          r)
