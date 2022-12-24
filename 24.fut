-- Absolutely delightful.  A proper BFS might be more efficient, but
-- the stencil works nicely too.

import "utils"

def parse s =
  let (get,ls) = lines.lines s
  let m = length (get ls[0]) - 2
  let on_line l = get l |> drop 1 |> take m
  in map on_line ls[1:length ls-1]

def easts wind = map (map (u8.=='>')) wind
def wests wind = map (map (u8.=='<')) wind
def norths wind = map (map (u8.=='^')) wind
def souths wind = map (map (u8.=='v')) wind

-- Windy cells after i steps.
def windy_in (i: i64) (wind: [][]u8) =
  map4 (map4 (\a b c d -> a||b||c||d))
       (map (rotate (-i)) (easts wind))
       (map (rotate i) (wests wind))
       (rotate i (norths wind))
       (rotate (-i) (souths wind))

#[noinline]
def step [n][m] (start: (i64,i64)) (wind: [n][m]u8) (s: i64) (where: [n][m]bool): [n][m]bool =
  let present i j = (i,j) == start || (i >= 0 && i < n && j >= 0 && j < m && where[i,j])
  let windy = windy_in s wind
  in tabulate_2d n m
                 (\i j -> !windy[i,j] &&
                          (present i j || present (i-1) j || present (i+1) j
                           || present i (j-1) || present i (j+1)))

entry part1 s =
  let [n][m] wind : [n][m]u8 = parse s
  let go = true
  let s = 1
  let where = replicate n (replicate m false)
  let (_,s,_) =
    loop (go,s,where) while go do
    let where = step (-1,0) wind s where
    in (!where[n-1,m-1], s+1, where)
  in s

entry part2 s =
  let [n][m] wind : [n][m]u8 = parse s
  let go = true
  let s = 1
  let where = replicate n (replicate m false)
  let (_,s,_) =
    loop (go,s,where) while go do
    let where = step (-1,0) wind s where
    in (!where[n-1,m-1], s+1, where)
  let (_,s,_) =
    loop (go,s,where) while go do
    let where = step (n,m-1) wind s where
    in (!where[0,0], s+1, where)
  let (_,s,_) =
    loop (go,s,where) while go do
    let where = step (-1,0) wind s where
    in (!where[n-1,m-1], s+1, where)
  in s


-- ==
-- entry: part1
-- input @ data/24.input
-- output { 373i64 }

-- ==
-- entry: part2
-- input @ data/24.input
-- output { 997i64 }
