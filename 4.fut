-- This is where parsing becomes annoying.

import "utils"

def span p xs =
  split (loop i = 0 while i < length xs && !p xs[i] do i + 1) xs

def parse (s: string[]) =
  let (get,ls) = lines.lines s
  in map (\l -> let (a,b) = span (==',') (get l)
                let (a0,a1) = span (=='-') a
                let (b0,b1) = span (=='-') (tail b)
                in ((atoi a0, atoi (tail a1)),
                    (atoi b0, atoi (tail b1)))) ls

let contains ((a:i32,b:i32),(x,y)) = (a <= x && b >= y) || (x <= a && y >= b)

entry part1 s =
  s |> parse |> count contains

let overlap ((a:i32,b:i32),(x,y)) =
  (a <= x && b >= x) || (a <= y && b >= y) || contains ((a,b),(x,y))

entry part2 s =
  s |> parse |> count overlap

-- ==
-- entry: part1
-- input @ data/4.input
-- output { 542 }

-- ==
-- entry: part2
-- input @ data/4.input
-- output { 900 }
