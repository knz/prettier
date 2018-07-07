-- A prettier printer
-- A note by Philip Wadler from 1997-1998, presented at the Symposium
-- "The Fun of Programming"
-- https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

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

data  Doc                =  Nil
                         |  String `Text` Doc
                         |  Int `Line` Doc

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

-- The <|> operator forms the union of the two sets of layouts.
-- The flatten operator replaces each line break (and its associated indentation) by a single space.
-- A document always represents a non-empty set of layouts,
-- where all layouts in the set flatten to the same layout.
-- As an invariant, we require in (x <|> y) that all layouts in x and y flatten to the same layout.

flatten NIL              =  NIL
flatten (x :<> y)        =  flatten x :<> flatten y
flatten (NEST i x)       =  NEST i (flatten x)
flatten (TEXT s)         =  TEXT s
flatten LINE             =  TEXT " "
flatten (x :<|> y)       =  flatten x

-- Given a document, representing a set of layouts, group returns the set with one
-- new element added, representing the layout in which everything is compressed
-- on one line. This is achieved by replacing each newline (and the corresponding
-- indentation) with text consisting of a single space. (Variants might be considered
-- where each newline carries with it the alternate text it should be replaced by. For
-- instance, some newlines might be replaced by the empty text, others with a single
-- space.)

group x                  =  flatten x :<|> x

-- Finally, the function layout converts a document to a string.

layout Nil               =  ""
layout (s `Text` x)      =  s ++ layout x
layout (i `Line` x)      =  '\n' : spaces i ++ layout x

spaces n = copy n ' '
copy i x                 =  [ x | _ <- [1..i] ]

-- Next, it is necessary to choose the best among the set of possible layouts. This
-- is done with a function best, which takes a document that may contain unions, and
-- returns a document containing no unions. A moment’s thought reveals that this
-- operation requires two additional parameters: one specifies the available width
-- w, and the second specifies the number of characters k already placed on the current
-- line (including indentation).

best :: Int -> Int -> DOC -> Doc
best w k x               =  be w k [(0,x)]

be w k []                =  Nil
be w k ((i,NIL):z)       =  be w k z
be w k ((i,x :<> y):z)   =  be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z)  =  be w k ((i+j,x):z)
be w k ((i,TEXT s):z)    =  s `Text` be w (k+length s) z
be w k ((i,LINE):z)      =  i `Line` be w i z
be w k ((i,x :<|> y):z)  =  better w k (be w k ((i,x):z))
                                       (be w k ((i,y):z))


-- The two middle cases [LINE/TEXT] adjust the current position: for a newline it is set to the
-- indentation, and for text it is incremented by the string length. For a union, the
-- better of the best of the two options is selected. It is essential for efficiency that
-- the inner computation of best is performed lazily. By the invariant for unions, no
-- first line of the left operand may be shorter than any first line of the right operand.
-- Hence, by the criterion given previously, the first operand is preferred if it fits, and
-- the second operand otherwise.

better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y           =  if fits (w-k) x then x else y

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
fits w (i `Line` x)      =  True

-- To pretty print a document one selects the best layout and converts it
-- to a string.

pretty w x               =  layout (best w 0 x)

-- Utility functions

x <+> y                  =  x <> text " " <> y
x </> y                  =  x <> line <> y

folddoc f []             =  nil
folddoc f [x]            =  x
folddoc f (x:xs)         =  f x (folddoc f xs)

spread                   =  folddoc (<+>)

stack                    =  folddoc (</.>)

NIL </.> NIL = NIL
NIL </.> y   = y
x </.> NIL   = x
x </.> y     = x </> y

nestunder i name nested  = group $ name <> (nest i (line <> group nested))

join    s l        = joindoc (text s <> line) l
joindoc s []       = nil
joindoc s [x]      = x
joindoc s (NIL:xs) = joindoc s xs
joindoc s (x:xs)   = x <> s <> joindoc s xs

joingroup i name divider = (nestunder i (text name)) . (join divider)

joinnestedright :: Int -> DOC -> [DOC] -> DOC
joinnestedright i sep []     = NIL
joinnestedright i sep [x]    = x
joinnestedright i sep (x:xs) = x <> (folddoc (<>) items)
    where
      items :: [DOC]
      items = map grouper xs
      grouper :: DOC -> DOC
      grouper a = line <> sep <+> (nest i $ group a)


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

testtree w               =  putStr (pretty w (showTree tree))
testtree' w              =  putStr (pretty w (showTree' tree))

-- SQL example

data SQLRel = Select [SQLScalar] [SQLTableExpr] SQLOrderBy SQLLimit
data SQLTableExpr = Table String
                  | PSelect SQLRel

type SQLOrderBy = Maybe [SQLScalar]
type SQLLimit = Maybe SQLScalar
data SQLScalar = SVar String
               | SNum Int
               | SQLScalar `SAdd` SQLScalar
               | Subquery SQLRel
               | Star

showSQL :: SQLRel -> DOC
showSQL (Select es from orderby limit) =
  group $ stack [ nestunder 2 (text "SELECT") (join "," $ map showSQLv es)
                , showSQLf from
                , showSQLo orderby
                , showSQLl limit]

showSQLf :: [SQLTableExpr] -> DOC
showSQLf [] = nil
showSQLf l  = joingroup 2 "FROM" "," $ map showSQLte l

showSQLte :: SQLTableExpr -> DOC
showSQLte (Table s)   = text s
showSQLte (PSelect s) = bracket "(" (showSQL s) ")"

showSQLo :: (Maybe [SQLScalar]) -> DOC
showSQLo Nothing   = nil
showSQLo (Just []) = nil
showSQLo (Just l)  = joingroup 2 "ORDER BY" "," $ map showSQLv l

showSQLl :: (Maybe SQLScalar) -> DOC
showSQLl Nothing  = nil
showSQLl (Just l) = nestunder 2 (text "LIMIT") $ showSQLv l

showSQLv :: SQLScalar   -> DOC
showSQLv (SVar s)       = text s
showSQLv (SNum i)       = text $ show i
showSQLv (a `SAdd` b)   = joinnestedright 2 (text "+") (flattenSQL a ++ flattenSQL b)
showSQLv (Subquery r)   = bracket "(" (showSQL r) ")"
showSQLv (Star)         = text "*"

flattenSQL :: SQLScalar -> [DOC]
flattenSQL (a `SAdd` b) = flattenSQL a ++ flattenSQL b
flattenSQL rest         = [showSQLv rest]

sql = Select
  [Star, SNum 3 `SAdd` (SVar "x") `SAdd` (Subquery sq)] -- exprs
  [Table "t"] -- from
  (Just [SNum 3, SNum 2, SNum 1]) -- order by
  (Just (SNum 123)) -- limit
sq = Select
   [SVar "k" `SAdd` SVar "v"]
   [Table "kv"]
   Nothing
   Nothing

testSQL w                =  putStrLn (pretty w (showSQL sql))

main = do
  putStrLn $ copy 5 '-'
  testSQL 5
  putStrLn $ copy 10 '-'
  testSQL 10
  putStrLn $ copy 30 '-'
  testSQL 30
  putStrLn $ copy 80 '-'
  testSQL 80

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
testXML w                =  putStr (pretty w (showXML xml))

-- main = testXML 80
