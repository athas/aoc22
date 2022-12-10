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

def coords moves = scan pos_add (0,0) moves

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
--
-- Represented as a single 64-bit number, with 4 bits per field.
type perm = u64

def tovec (xs: [9]u64) : perm =
  let f i = xs[i32.u64 i]<<(i*4)
  in f 0 | f 1 | f 2 | f 3 | f 4 | f 5 | f 6 | f 7 | f 8

def perm_lookup (i: u64) (x: perm) : u64 =
  (x >> (i * 4)) & 0b1111

def perm (d: dir): perm =
  match d case #U  -> tovec [3,4,5, 6,7,8, 7,7,7]
          case #D  -> tovec [1,1,1, 0,1,2, 3,4,5]
          case #L  -> tovec [1,2,5, 4,5,5, 7,8,5]
          case #R  -> tovec [3,0,1, 3,3,4, 3,6,7]
          case #UL -> tovec [4,5,5, 7,8,5, 7,7,8]
          case #DL -> tovec [1,1,2, 1,2,5, 4,5,5]
          case #UR -> tovec [3,3,4, 3,6,7, 6,7,7]
          case #DR -> tovec [0,1,1, 3,0,1, 3,3,4]
          case #C  -> tovec [0,1,2, 3,4,5, 6,7,8]

def perm_compose (a: perm) (b: perm): perm =
  tovec [perm_lookup (perm_lookup 0 a) b,
         perm_lookup (perm_lookup 1 a) b,
         perm_lookup (perm_lookup 2 a) b,
         perm_lookup (perm_lookup 3 a) b,
         perm_lookup (perm_lookup 4 a) b,
         perm_lookup (perm_lookup 5 a) b,
         perm_lookup (perm_lookup 6 a) b,
         perm_lookup (perm_lookup 7 a) b,
         perm_lookup (perm_lookup 8 a) b]

def idx_to_dir (x: u64) : dir =
  ([#UL, #U, #UR, #L, #C, #R, #DL, #D, #DR])[i32.u64 x]

def move_tail dirs =
  map perm dirs
  |> scan perm_compose (perm #C)
  |> map (perm_lookup 4)
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
  let heads = coords (map move dirs)
  let (tails, _) = compute_tail (heads,dirs)
  in count_visits tails

entry part2 s =
  let dirs = moves s
  let heads = coords (map move dirs)
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
