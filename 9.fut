-- This one is truly crazy, but I got it to work!  The 'unmove'
-- function is pretty inelegant, but it does not affect the
-- parallelism.

import "utils"
import "lib/github.com/diku-dk/segmented/segmented"

type dir = #U | #D | #L | #R | #UL | #UR | #DL | #DR | #C

def dir (c: u8) : dir =
  match c
  case 'U' -> #U
  case 'D' -> #D
  case 'L' -> #L
  case _   -> #R

def testinput = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n"

def parse (s: string[]) =
  let (get,ls) = lines.lines s
  in map (\l -> let l' = get l
                in (dir l'[0], atoi (drop 2 l'))) ls

type pos = (i32,i32)

def pos_add (a: pos) (b: pos) = (a.0 + b.0, a.1 + b.1)
def pos_sub (a: pos) (b: pos) = (a.0 - b.0, a.1 - b.1)

def move (d: dir) : pos =
  match d case #U -> (0,1)
          case #D -> (0,-1)
          case #L -> (-1,0)
          case #R -> (1,0)
          case #UL -> (-1,1)
          case #DL -> (-1,-1)
          case #UR -> (1,1)
          case #DR -> (1,-1)
          case #C -> (0,0)

def unmove (p: pos) : dir =
  match p case (0,1) -> #U
          case (0,-1) -> #D
          case (-1,0) -> #L
          case (1,0) -> #R
          case (-1,1) -> #UL
          case (-1,-1) -> #DL
          case (1,1) -> #UR
          case (1,-1) -> #DR
          case _ -> #C

-- Representing neighbourhood with indexes
--
-- 0 1 2
-- 3 4 5
-- 6 7 8
type movvec = [9]i8

def movvec (d: dir): movvec =
  match d case #U  -> [3,4,5, 6,7,8, 7,7,7]
          case #D  -> [1,1,1, 0,1,2, 3,4,5]
          case #L  -> [1,2,5, 4,5,5, 7,8,5]
          case #R  -> [3,0,1, 3,3,4, 3,6,7]
          case #UL -> [4,5,5, 7,8,5, 7,7,8]
          case #DL -> [1,1,2, 1,2,5, 4,5,5]
          case #UR -> [3,3,4, 3,6,7, 6,7,7]
          case #DR -> [0,1,1, 3,0,1, 3,3,4]
          case #C  -> [0,1,2, 3,4,5, 6,7,8]

def movvec_compose (a: movvec) (b: movvec): movvec =
  [b[a[0]], b[a[1]], b[a[2]],
   b[a[3]], b[a[4]], b[a[5]],
   b[a[6]], b[a[7]], b[a[8]]]

def idx_to_dir (x: i8) : dir =
  match x case 0 -> #UL
          case 1 -> #U
          case 2 -> #UR
          case 3 -> #L
          case 4 -> #C
          case 5 -> #R
          case 6 -> #DL
          case 7 -> #D
          case _ -> #DR

def move_tail dirs =
  map movvec dirs
  |> scan movvec_compose (movvec #C)
  |> map (.[4])
  |> map idx_to_dir

def count_visits poses =
  let min_x = i32.minimum (map (.0) poses)
  let max_x = i32.maximum (map (.0) poses)
  let min_y = i32.minimum (map (.1) poses)
  let max_y = i32.maximum (map (.1) poses)
  let span_x = max_x - min_x + 1
  let span_y = max_y - min_y + 1
  let flat_pos (x,y) = i64.i32 ((x-min_x) * span_y + (y-min_y))
  in hist (+) 0i32 (i64.i32 (span_x*span_y))
          (map flat_pos poses)
          (map (const 1) poses)
     |> count (>0)

def moves s = s |> parse |> expand (\(_,n) -> i64.i32 n) (\(d,_) _ -> d)

let compute_tail (heads, dirs) =
  let tails = move_tail dirs |> map move |> map2 pos_add heads
  let tail_dir i prev this: dir =
    let prev = if i == 0 then (0,0) else prev
    in unmove (pos_sub this prev)
  in (tails, map3 tail_dir (indices tails) (rotate (-1) tails) tails)

entry part1 s =
  let dirs = moves s
  let heads = scan pos_add (0,0) (map move dirs)
  let (tails, _) = compute_tail (heads,dirs)
  in count_visits tails

entry part2 s =
  let dirs = moves s
  let heads = scan pos_add (0,0) (map move dirs)
  let (tails, _) = iterate 9 compute_tail (heads,dirs)
  in count_visits tails

-- ==
-- entry: part1
-- input @ data/9.input
-- output { 5779 }

-- ==
-- entry: part2
-- input @ data/9.input
-- output { 2331 }
