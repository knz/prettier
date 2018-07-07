# prettier
Experiments with Philip Waddler's prettier pretty-printer

This adds support for right-alignment, as is useful for SQL.

See the output of the included example:

```
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SELECT *, 3 + x + ( SELECT k + v FROM kv ) FROM t, u, v, ( SELECT k + v FROM kv ) ORDER BY 3, 2, 1, wow, yay LIMIT 123

--------------------------------------------------------------------------------
  SELECT *, 3 + x + ( SELECT k + v FROM kv )
    FROM t, u, v, ( SELECT k + v FROM kv )
ORDER BY 3, 2, 1, wow, yay
   LIMIT 123

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
ORDER BY 3, 2, 1, wow, yay
   LIMIT 123

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
ORDER BY
  3,
  2,
  1,
  wow,
  yay
LIMIT
  123
```
