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
  let f (w,i) =
    if w |> map (\x -> u32.u8 x - 'a') |> map (1<<) |> reduce (^) 0 |> u32.popc |> (==14)
    then i32.i64 i + 14 else i32.highest
  in windows 14 s |> (id &&& indices) |> uncurry zip |> map f |> i32.minimum

-- ==
-- entry: part1
-- input @ data/6.input
-- output { 1155 }

-- ==
-- entry: part2
-- input @ data/6.input
-- output { 2789 }
