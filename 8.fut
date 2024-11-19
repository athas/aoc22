-- This one was lovely.  The first part is a classic use of scans, and
-- the second was... tolerable.  I do wonder if it is possible to also
-- parallelise the search.

import "utils"

def visible (xs: []i32) =
  xs |> (id &&& exscan i32.max i32.lowest) |> uncurry (map2 (>))

def parse [l] (s: string [l]) =
  -- Careful to cut off the newlines.
  let n = i64.f32 (f32.sqrt (f32.i64 l))
  in sized (n*(n+1)) s
     |> unflatten
     |> map (take n)
     |> map (map (\x -> x - '0'))
     |> map (map i32.u8)

entry part1 s =
  let grid = parse s
  in map4 (map4 (\a b c d -> a || b || c || d))
          (map visible grid)
          (map reverse (map visible (map reverse grid)))
          (transpose (map visible (transpose grid)))
          (transpose (map reverse (map visible (map reverse (transpose grid)))))
     |> flatten |> map i32.bool |> i32.sum

def search [n] (grid: [n][n]i32) (i,j) (x,y) : i32 =
  let me = grid[i,j]
  let (_,_,view) =
    loop (i,j,acc) = (i+x,j+y,0) while (i>=0 && i < n && j >= 0 && j < n) do
    let h = grid[i,j]
    in if h >= me then (-1,-1,acc+1) else (i+x,j+y,acc+1)
  in view

entry part2 s =
  let [n] (grid: [n][n]i32) = parse s
  in tabulate_2d n n (\i j -> search grid (i,j) (-1,0) *
                              search grid (i,j) (1,0) *
                              search grid (i,j) (0,-1) *
                              search grid (i,j) (0,1))
     |> flatten |> i32.maximum

-- ==
-- entry: part1
-- input @ data/8.input
-- output { 1713 }

-- ==
-- entry: part2
-- input @ data/8.input
-- output { 268464 }
