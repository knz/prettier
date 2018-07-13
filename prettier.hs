-- A prettier printer
-- A note by Philip Wadler from 1997-1998, presented at the Symposium
-- "The Fun of Programming"
-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

-- With ideas from Daniel Mendler,
-- https://github.com/minad/wl-pprint-annotated/blob/master/src/Text/PrettyPrint/Annotated/WL.hs

-- Modified for use with SQL.
-- Matt Jibson & Raphael 'kena' Poss

import System.Environment
import Data.List.Utils (replace)

-- The pretty printer

infixr 5                 :<|>
infixr 6                 :<>
infixr 6                 <>

data  DOC                =  NIL
                         |  DOC :<> DOC
                         |  NEST Int DOC
                         |  TEXT String
                         |  LINE
                         |  DOC :<|> DOC
                         |  COLUMN (Int -> DOC)
                         |  NESTING (Int -> DOC)
                         |  PAD Int

data  Doc                =  Nil
                         |  String `Text` Doc
                         |  Fil `Line` Doc

-- Here <> is the associative operation that concatenates two documents,
-- which has the empty document nil as its left and right unit.
-- The function text converts a string to the corresponding document, and
-- the document line denotes a line break;
-- we adopt the convention that the string passed to text does not contain newline characters,
-- so that line is always used for this purpose.
-- The function nest adds indentation to a document.

nil                      =  NIL

x <> y                   =  x :<> y

nest i x                 =  NEST i x

text s                   =  TEXT s

line                     =  LINE


-- Extra alignment operators
-- Ideas from https://github.com/minad/wl-pprint-annotated/blob/master/src/Text/PrettyPrint/Annotated/WL.hs

align :: DOC -> DOC
align d = COLUMN $
          \k -> NESTING $
          \i -> nest (k - i) d

-- The <|> operator forms the union of the two sets of layouts.
-- The flatten operator replaces each line break (and its associated indentation) by a single space.
-- A document always represents a non-empty set of layouts,
-- where all layouts in the set flatten to the same layout.
-- As an invariant, we require in (x <|> y) that all layouts in x and y flatten to the same layout.

flatten :: DOC -> DOC
flatten NIL              =  NIL
flatten (x :<> y)        =  flatten x :<> flatten y
flatten (NEST i x)       =  NEST i (flatten x)
flatten (TEXT s)         =  TEXT s
flatten LINE             =  TEXT " "
flatten (x :<|> y)       =  flatten x
flatten (COLUMN f)       =  COLUMN (flatten . f)
flatten (NESTING f)      =  NESTING (flatten . f)
flatten (PAD n)          =  NIL

-- Given a document, representing a set of layouts, group returns the set with one
-- new element added, representing the layout in which everything is compressed
-- on one line. This is achieved by replacing each newline (and the corresponding
-- indentation) with text consisting of a single space. (Variants might be considered
-- where each newline carries with it the alternate text it should be replaced by. For
-- instance, some newlines might be replaced by the empty text, others with a single
-- space.)

group :: DOC -> DOC
group x                  =  flatten x :<|> x

-- Finally, the function layout converts a document to a string.
-- The first parameter is the width of a tab in spaces.

layout :: Int -> Doc -> String
layout tw Nil                  =  ""
layout tw (s       `Text` x)   =  s ++ (layout tw x)
layout tw ((Fil i) `Line` x)   =  ('\n' : tabspaces tw i) ++ (layout tw x)

spaces :: Int -> String
spaces n = copy n ' '

tabspaces :: Int -> Int -> String
tabspaces tw n = (copy (n `div` tw) '\t') ++ (spaces (n `mod` tw))

copy :: Int -> Char -> String
copy i x                 =  [ x | _ <- [1..i] ]

-- Next, it is necessary to choose the best among the set of possible layouts. This
-- is done with a function best, which takes a document that may contain unions, and
-- returns a document containing no unions. A moment’s thought reveals that this
-- operation requires two additional parameters: one specifies the available width
-- w, and the second specifies the number of characters k already placed on the current
-- line (including indentation).


-- Cfg is the configuration for a layout.
-- The int is the desired line length.
data Cfg = Cfg Int
-- lw accesses the line length.
lw :: Cfg -> Int
lw (Cfg x) = x

-- Cur is the current output position on the current line.
-- The int is the column number.
data Cur = Cur Int
-- cur accesses the cursor.
cur :: Cur -> Int
cur (Cur k) = k

-- Fil is the current filling at the beginning of a new line.
-- The int is the number of spaces.
data Fil = Fil Int
-- fil accesses the filling width.
fil :: Fil -> Int
fil (Fil x) = x

best :: Cfg -> DOC -> Doc
best w x               =  be w (Cur 0) [(Fil 0,x)]

be :: Cfg -> Cur -> [(Fil,DOC)] -> Doc
be w k []                          =  Nil
be w k ((i,NIL)      :z)  =  be w k z
be w k ((i,x :<> y)  :z)  =  be w k ((i,x):(i,y):z)
be w k ((i,NEST j x) :z)  =  be w k ((Fil (fil i + j),x):z)
be w k ((i,TEXT s)   :z)  =  s `Text` be w (Cur (cur k + length s)) z
be w k ((i,LINE)     :z)  =  i `Line` be w (Cur (fil i)) z
be w k ((i,x :<|> y) :z)  =  better w k (be w k ((i,x):z))
                                        (be w k ((i,y):z))
be w k ((i,COLUMN f) :z)  =  be w k ((i,(f $ cur k)):z)
be w k ((i,NESTING f):z)  =  be w k ((i,(f $ fil i)):z)
be w k ((i,PAD n)    :z)  =  (spaces n) `Text` be w (Cur (cur k + n)) z

-- The two middle cases [LINE/TEXT] adjust the current position: for a newline it is set to the
-- indentation, and for text it is incremented by the string length. For a union, the
-- better of the best of the two options is selected. It is essential for efficiency that
-- the inner computation of best is performed lazily. By the invariant for unions, no
-- first line of the left operand may be shorter than any first line of the right operand.
-- Hence, by the criterion given previously, the first operand is preferred if it fits, and
-- the second operand otherwise.

better :: Cfg -> Cur -> Doc -> Doc -> Doc
better w k x y           =  if fits ((lw w)-(cur k)) x then x else y

-- If the available width is less than zero, then the document cannot fit. Otherwise,
-- if the document is empty or begins with a newline then it fits trivially, while if
-- the document begins with text then it fits if the remaining document fits in the
-- remaining space. Handling negative widths is not merely esoteric, as the case for
-- text may yield a negative width. No case is required for unions, since the function
-- is only applied to the best layout of a set.

fits :: Int -> Doc -> Bool
fits w x | w < 0         =  False
fits w Nil               =  True
fits w (s `Text` x)      =  fits (w - length s) x
fits w (_ `Line` x)      =  True

-- To pretty print a document one selects the best layout and converts it
-- to a string.

pretty :: Int -> Cfg -> DOC -> String
pretty iw w x               =  layout iw (best w x)

-- Utility functions

x <+> y                  =  x <> text " " <> y
x </> y                  =  x <> line <> y

folddoc f []             =  nil
folddoc f [x]            =  x
folddoc f (x:xs)         =  f x (folddoc f xs)

folddocmap f g           = (folddoc f) . (map g)
concatdocmap             = folddocmap (<>)

spread                   =  folddoc (<+>)

stack                    =  folddoc (</>)

NIL </.> NIL = NIL
NIL </.> y   = y
x </.> NIL   = x
x </.> y     = x </> y

-- rltable creates a document that formats a list of pairs of items either:
--
--  - as a 2-column table, with the left column right-aligned and the right
--    column left-aligned, for example:
--       SELECT aaa
--              bbb
--         FROM ccc
--
--  - as sections, for example:
--       SELECT
--           aaa
--           bbb
--       FROM
--           ccc
--
-- We restrict the left value in each list item to be a one-line string
-- to make the width computation efficient.
rltable :: Int -> [(String,DOC)] -> DOC
rltable _ []  = NIL
rltable i items = group $ alignedtable :<|> nestedsections
  where
    -- alignedtable produces the middle alignment.
    -- We use left padding on each row to align the titles on the right.
    alignedtable :: DOC
    alignedtable = stack' $ map row items
      where
        -- leftwidth is the maximum width of the entire left column.
        leftwidth :: Int
        leftwidth = maximum $ map (length . fst) items
        -- padwidth s is the amount of spaces to prefix to s so that
        -- the total becomes leftwidth.
        padwidth :: String -> Int
        padwidth s = leftwidth - (length s)

        -- alignedrow renders ws, then right-aligns lbl, then formats doc after it so that
        -- every line after the first is left-aligned to the right of lbl.
        row :: (String, DOC) -> DOC
        row (lbl, doc) = (PAD (padwidth lbl)) <> (text lbl) <+> (align $ group doc)

    nestedsections :: DOC
    nestedsections = stack' $ map row items
      where
        row :: (String, DOC) -> DOC
        row (lbl, d) = (text lbl) <> (nest i $ line <> group d)

    stack' :: [DOC] -> DOC
    stack' = folddoc (</>)

join    s l        = joindoc (text s <> line) l
joindoc s []       = nil
joindoc s [x]      = x
joindoc s (NIL:xs) = joindoc s xs
joindoc s (x:xs)   = x <> s <> joindoc s xs

joinnestedright :: Int -> String -> [DOC] -> DOC
joinnestedright i sep []     = NIL
joinnestedright i sep [x]    = x
joinnestedright i sep (x:xs) = x <> (folddoc (<>) items)
    where
      sepdoc = text sep
      items :: [DOC]
      items = map grouper xs
      grouper :: DOC -> DOC
      grouper a = line <> (text sep) <+> (nest i $ group a)

-- joinnestedright' joins the list of DOCs using the delimiter given as string,
-- and if the output needs to wraps ensures that the delimiter is right-aligned
-- on every line after the first.
--
-- For example:
--        hello
--      + world
--      + sometext
--
joinnestedright' :: String -> [DOC] -> DOC
joinnestedright' sep []     = NIL
joinnestedright' sep (x:xs) = x <> nest (-seplen-1) (folddoc (<>) items)
    where
      sepdoc = text sep
      seplen = length sep
      items :: [DOC]
      items = map grouper xs
      grouper :: DOC -> DOC
      grouper a = line <> sepdoc <+> (align $ group a)



-- Often a layout consists of an opening bracket, an indented portion, and a
-- closing bracket.

bracket l x r            =  group (text l <>
                                   nest 2 (line <> x) <>
                                   line <> text r)

-- The function fillwords takes a string, and returns a document that fills each
-- line with as many words as will fit. It uses words, from the Haskell standard library,
-- to break a string into a list of words.
-- Recall that we do not expose :<|> to the user, but x <+/> y may be safely exposed,
-- because it satisfies the invariant required by :<|>. Both text " " and line
-- flatten to the same layout, and the former has a longer first line than the latter.
-- Alternatively, one can rewrite the above by noting that (text " " :<|> line)
-- is equivalent to (group line).

x <+/> y                 =  x <> (text " " :<|> line) <> y
fillwords                =  folddoc (<+/>) . map text . words

-- A variant of fillwords is fill, which collapses a list of documents into a
-- document. It puts a space between two documents when this leads to reason-
-- able layout, and a newline otherwise. (I stole this function from Peyton Jones’s
-- expansion of Hughes’s pretty printer library.)
-- Note the use of flatten to ensure that a space is only inserted between documents
-- which occupy a single line. Note also that the invariant for :<|>
-- is again satisfied.

fill []                  =  nil
fill [x]                 =  x
fill (x:y:zs)            =  (flatten x <+> fill (flatten y : zs))
                            :<|>
                            (x </> fill (y : zs))
-- Tree example

data  Tree               =  Node String [Tree]

showTree (Node s ts)     =  group (text s <> nest (length s) (showBracket ts))

showBracket []           =  nil
showBracket ts           =  text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees [t]            =  showTree t
showTrees (t:ts)         =  showTree t <> text "," <> line <> showTrees ts

showTree' (Node s ts)    =  text s <> showBracket' ts

showBracket' []          =  nil
showBracket' ts          =  bracket "[" (showTrees' ts) "]"

showTrees' [t]           =  showTree t
showTrees' (t:ts)        =  showTree t <> text "," <> line <> showTrees ts

tree                     =  Node "aaa" [
  Node "bbbbb" [
      Node "ccc" [],
      Node "dd" []
      ],
  Node "eee" [],
  Node "ffff" [
      Node "gg" [],
      Node "hhh" [],
      Node "ii" []
      ]
  ]

testtree w               =  putStr (pretty 4 w (showTree tree))
testtree' w              =  putStr (pretty 4 w (showTree' tree))

-- SQL example

data SQLRel = Select [SQLScalar] [SQLTableExpr] SQLWhere SQLOrderBy SQLLimit
data SQLTableExpr = Table String
                  | PSelect SQLRel

type SQLOrderBy = Maybe [SQLScalar]
type SQLLimit = Maybe SQLScalar
type SQLWhere = Maybe SQLScalar
data SQLScalar = SVar String
               | SNum Int
               | SQLScalar `SAdd` SQLScalar
               | SQLScalar `SAnd` SQLScalar
               | Subquery SQLRel
               | Star

showSQL :: SQLRel -> DOC
showSQL (Select es from wh orderby limit) = group $ rltable 2 items
  where items =
          [("SELECT", (join "," $ map showSQLv es))] ++
          showSQLf from ++
          showSQLw wh ++
          showSQLo orderby ++
          showSQLl limit

showSQLw :: (Maybe SQLScalar) -> [(String, DOC)]
showSQLw Nothing  = []
showSQLw (Just w) = [("WHERE", showSQLv w)]

showSQLf :: [SQLTableExpr] -> [(String, DOC)]
showSQLf [] = []
showSQLf l  = [("FROM", join "," $ map showSQLte l)]

showSQLte :: SQLTableExpr -> DOC
showSQLte (Table s)   = text s
showSQLte (PSelect s) = bracket "(" (showSQL s) ")"

showSQLo :: (Maybe [SQLScalar]) -> [(String, DOC)]
showSQLo Nothing   = []
showSQLo (Just []) = []
showSQLo (Just l)  = [("ORDER BY", join "," $ map showSQLv l)]

showSQLl :: (Maybe SQLScalar) -> [(String, DOC)]
showSQLl Nothing  = []
showSQLl (Just l) = [("LIMIT", showSQLv l)]

showSQLv :: SQLScalar   -> DOC
showSQLv (SVar s)       = text s
showSQLv (SNum i)       = text $ show i
showSQLv (a `SAdd` b)   = joinnestedright 2 "+" (flattenSQL1 a ++ flattenSQL1 b)
showSQLv (a `SAnd` b)   = joinnestedright' "AND" (flattenSQL2 a ++ flattenSQL2 b)
showSQLv (Subquery r)   = bracket "(" (showSQL r) ")"
showSQLv (Star)         = text "*"

flattenSQL1 :: SQLScalar -> [DOC]
flattenSQL1 (a `SAdd` b) = flattenSQL1 a ++ flattenSQL1 b
flattenSQL1 rest         = [showSQLv rest]

flattenSQL2 :: SQLScalar -> [DOC]
flattenSQL2 (a `SAnd` b) = flattenSQL2 a ++ flattenSQL2 b
flattenSQL2 rest         = [showSQLv rest]

sql = Select
  [Star, SNum 3 `SAdd` (SVar "x") `SAdd` (Subquery sq)] -- exprs
  [Table "t", Table "u", Table "v", PSelect sq] -- from
  (Just ((SVar "some" `SAdd` SNum 2 `SAdd` SNum 4) `SAnd` SVar "more" `SAnd` SVar "stuff" `SAnd` (SNum 1 `SAdd` (SVar "x") `SAdd` SNum 23))) -- where
  (Just [SNum 3, SNum 2, SNum 1, SVar "wow", SVar "yay"]) -- order by
  (Just (SNum 123)) -- limit

sq = Select
   [SVar "k" `SAdd` SVar "v"]
   [Table "kv"]
   Nothing
   Nothing
   Nothing

testSQL :: Int -> Int -> String
testSQL tw w =  pretty tw (Cfg w) (showSQL sql)

-- This main function exercises the pretty printer. It accepts 4
-- command-line arguments.
--
-- The first two argument control the first phase, pretty-printing proper.
-- This first phasae always emits tab characters.
--  - the desired line width.
--  - the virtual tab width to use when producing tabs and computing
--    whether tabs fit.
--
-- The 3rd argument determines whether to apply a second phase or not.
-- If the 3rd argument is missing or empty, then the second phase
-- is not applied and the output of the first phase is printed as-is.
--
-- If the 3rd argument is specified, this enables the second phase:
-- replacing tabs by spaces as would be done by an editor or web
-- browser or terminal. The 3rd argument indicates the character to
-- replace tabs with. It defaults to a space. Recommend value: _ (to
-- see the tabs more explicitly).
--
-- If the 4th argument is specified, it determines the physical tab
-- width used by the replacement phase. If omitted, it defaults to the
-- virtual tab width (i.e. assume the input to the first phase already
-- matches the output device). This argument is meant to simulate a
-- difference between the tab width assumed by the pretty-printing
-- code and the actual tab width of a display device.
--
-- A good pretty-printing algorithm should ensure the output is
-- pleasant to read even if the two values mismatch.
main :: IO ()
main = do
  args <- getArgs
  let (w, tw, doRepl, replChar, lw) = parse args
        where
          parse :: [String] -> (Int, Int, Bool, Char, Int)
          parse []                     = parse ("80":[])
          parse (cw:[])                = parse (cw:"4":[])
          parse (cw:ctw:[])            = parse (cw:ctw:"":[])
          parse (cw:ctw:cx:[])         = parse (cw:ctw:cx:ctw:[])
          parse (cw:ctw:[]:clw:_)      = ((read cw), (read ctw), False, ' ', (read clw))
          parse (cw:ctw:(crc:_):clw:_) = ((read cw), (read ctw), True, crc, (read clw))
    in
    do
      putStrLn $ copy w '-'
      putStrLn $ maybeReplace doRepl lw replChar $ testSQL tw w
        where
          maybeReplace False lw c s = s
          maybeReplace True  lw c s = replace ['\t'] (copy lw c) s

-- XML example

-- Here are functions that pretty print a simplified subset of XML containing
-- elements, attributes, and text.

data XML                 =  Elt String [Att] [XML]
                         |  Txt String
data Att                 =  Att String String

showXML x                =  folddoc (<>) (showXMLs x)
showXMLs (Elt n a [])    =  [text "<" <> showTag n a <> text "/>"]
showXMLs (Elt n a c)     =  [text "<" <> showTag n a <> text ">" <>
                             showFill showXMLs c <>
                             text "</" <> text n <> text ">"]
showXMLs (Txt s)         =  map text (words s)

showAtts (Att n v)       =  [text n <> text "=" <> text (quoted v)]

quoted s                 =  "\"" ++ s ++ "\""

showTag n a              =  text n <> showFill showAtts a

showFill f []            =  nil
showFill f xs            =  bracket "" (fill (concat (map f xs))) ""

xml                      =  Elt "p" [
  Att "color" "red",
  Att "font" "Times",
  Att "size" "10"
  ] [
  Txt "Here is some",
  Elt "em" [] [
      Txt "emphasized"
      ],
  Txt "text.",
  Txt "Here is a",
  Elt "a" [
      Att "href" "http://www.eg.com/"
      ] [
      Txt "link"
      ],
  Txt "elsewhere."
  ]
testXML w                =  putStr (pretty 4 w (showXML xml))

-- main = testXML 80
