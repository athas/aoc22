-- This one is quite clean.  The use of histograms is a bit clumsy
-- perhaps due to its sparsity, but it should be quite efficient.

import "utils"

entry part1 (s: string[]) =
  let f a b c d i =
    if a != b && a != c && a != d
       && b != c && b != d
       && c != d
    then i32.i64 i + 4 else i32.highest
  in map5 f s (rotate 1 s) (rotate 2 s) (rotate 3 s) (indices s) |> i32.minimum

entry part2 [n] (s: string[n]) =
  let f i =
    if i < n-13 &&
       all (<2) (hist (+) 0 26
                      (map (i64.u8 <-< (u8.-'a')) (take 14 (drop i s)))
                      (replicate 14 1))
    then i32.i64 i + 14 else i32.highest
  in map f (indices s) |> i32.minimum

-- ==
-- entry: part1
-- input @ data/6.input
-- output { 1155 }

-- ==
-- entry: part2
-- input @ data/6.input
-- output { 2789 }
