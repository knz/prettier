# A Wadler/Leijen Pretty Printer with right alignment

Experiments with Philip Wadler's prettier pretty-printer,
to add support for right-alignment, as is useful for SQL.

Work stimulated by [Matt Jibson](https://github.com/mjibson),
prototype hs code by Raphael 'kena' Poss, to prepare the algorithm for
use in [CockroachDB](https://github.com/cockroachdb/cockroach) and
[sequel fumpt](https://sqlfum.pt/).

Inspirations from:

- Philip Wadler, [A prettier printer](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
- Daan Leijen, [The Wadler/Leijen Pretty Printer](https://hackage.haskell.org/package/wl-pprint-1.2.1/docs/Text-PrettyPrint-Leijen.html)
- David Luposchainsky, [A modern Wadler/Leijen Pretty Printer](https://github.com/quchen/prettyprinter)
- Daniel Mendler, [Wadler/Leijen pretty printer with annotations](https://github.com/minad/wl-pprint-annotated/blob/master/src/Text/PrettyPrint/Annotated/WL.hs)



Salient functions:

```haskell
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
```

```haskell
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
```

We use this for SQL; see the output of the included example:

```sql
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SELECT *, 3 + x + ( SELECT k + v FROM kv ) FROM t, u, v, ( SELECT k + v FROM kv ) WHERE some AND more AND stuff AND 1 + x + 23 ORDER BY 3, 2, 1, wow, yay LIMIT 123
--------------------------------------------------------------------------------
  SELECT *, 3 + x + ( SELECT k + v FROM kv )
    FROM t, u, v, ( SELECT k + v FROM kv )
   WHERE some AND more AND stuff AND 1 + x + 23
ORDER BY 3, 2, 1, wow, yay
   LIMIT 123;
------------------------------
  SELECT *,
         3
         + x
         + (
             SELECT k + v
               FROM kv
           )
    FROM t,
         u,
         v,
         (
           SELECT k + v
             FROM kv
         )
   WHERE some
     AND more
     AND stuff
     AND 1 + x + 23
ORDER BY 3, 2, 1, wow, yay
   LIMIT 123;
---------------
  SELECT *,
         3
         + x
         + (
             SELECT
               k
               + v
             FROM
               kv
           )
    FROM t,
         u,
         v,
         (
           SELECT
             k
             + v
           FROM
             kv
         )
   WHERE some
     AND more
     AND stuff
     AND 1
         + x
         + 23
ORDER BY 3,
         2,
         1,
         wow,
         yay
   LIMIT 123;
-----
SELECT
  *,
  3
  + x
  + (
      SELECT
        k
        + v
      FROM
        kv
    )
FROM
  t,
  u,
  v,
  (
    SELECT
      k
      + v
    FROM
      kv
  )
WHERE
  some
AND more
AND stuff
AND 1
  + x
  + 23
ORDER BY
  3,
  2,
  1,
  wow,
  yay
LIMIT
  123
```
