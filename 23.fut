-- This was delightful.  I wanted to solve this with a spatial data
-- structure rather than a stencil, although the stencil will almost
-- certainly be more efficient due to the density and locality.  I
-- maintain the world as an array of the elf positions, encoded as a
-- u32.  I perform lookups by sorting and then doing a binary
-- search. I end up sorting much too often here.  I was worried that
-- part 2 would be something impossible to do with a stencil (also,
-- stencils are boring).

import "utils"
import "lib/github.com/diku-dk/sorts/merge_sort"

def testinput = "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#..\n"
def smallinput = ".....\n..##.\n..#..\n.....\n..##.\n.....\n"

type pos = u32

def pos_x (p: pos) = i16.u32 (p >> 16)
def pos_y (p: pos) = i16.u32 p
def mk_pos (x: i16) (y: i16) = (u32.i16 x << 16) | u32.i16 y

def pos_nw p = mk_pos (pos_x p - 1) (pos_y p + 1)
def pos_n p = mk_pos (pos_x p) (pos_y p + 1)
def pos_ne p = mk_pos (pos_x p + 1) (pos_y p + 1)
def pos_w p = mk_pos (pos_x p - 1) (pos_y p)
def pos_e p = mk_pos (pos_x p + 1) (pos_y p)
def pos_sw p = mk_pos (pos_x p - 1) (pos_y p - 1)
def pos_s p = mk_pos (pos_x p) (pos_y p - 1)
def pos_se p = mk_pos (pos_x p + 1) (pos_y p - 1)

type dir = #n | #s | #w | #e

#[noinline]
def occupied [n] (world: [n]pos) (p: pos) =
  let i = binsearch (<=) world p
  in i >= 0 && i < n && world[i] == p

-- This one is clever.
#[noinline]
def singleton [n] (world: [n]pos) (p: pos) =
  let i = binsearch (<=) world (p-1)
  in world[(i+1)%n] != world[(i+2)%n]

def propose [n] (world: [n]pos) (ds: [4]dir) (from: pos) : pos =
  if occupied world (pos_nw from) || occupied world (pos_n from) || occupied world (pos_ne from)
     || occupied world (pos_w from) || occupied world (pos_e from)
     || occupied world (pos_sw from) || occupied world (pos_s from) || occupied world (pos_se from)
  then let clear a b c = !occupied world a && !occupied world b && !occupied world c
       in (.0) <|
          loop (pos,i) = (from,0) while i < 4 do
            match ds[i] case #n -> if clear (pos_nw from) (pos_n from) (pos_ne from)
                                   then (pos_n from,4)
                                   else (pos,i+1)
                        case #s -> if clear (pos_sw from) (pos_s from) (pos_se from)
                                   then (pos_s from,4)
                                   else (pos,i+1)
                        case #w -> if clear (pos_nw from) (pos_w from) (pos_sw from)
                                   then (pos_w from,4)
                                   else (pos,i+1)
                        case #e -> if clear (pos_ne from) (pos_e from) (pos_se from)
                                   then (pos_e from,4)
                                   else (pos,i+1)
  else from

#[noinline]
def order (world: []pos) = merge_sort (<=) world

def parse s =
  let (get,ls) = lines.lines s
  let m = length (get ls[0])
  let on_line l = get l :> [m]u8
  let world = map on_line ls |> reverse
  let is = map (map (\(y,x) -> mk_pos (i16.i64 x) (i16.i64 y))) (indices_2d world)
  in map2 zip is world
     |> flatten
     |> filter (\(_,c) -> c == '#')
     |> map (.0)
     |> order

def dirs: []dir = [#n, #s, #w, #e]

#[noinline]
def step [n] (i: i64) (world: [n]pos) : [n]pos =
  let proposals = map (propose world (rotate i dirs)) world
  let proposals_sorted = order proposals
  let resolve from to = if singleton proposals_sorted to then to else from
  in order (map2 resolve world proposals)

entry part1 s =
  let world = parse s
  let world = loop world for i < 10 do step i world
  let min_x = world |> map pos_x |> i16.minimum
  let max_x = world |> map pos_x |> i16.maximum
  let min_y = world |> map pos_y |> i16.minimum
  let max_y = world |> map pos_y |> i16.maximum
  in (i32.i16 (max_x-min_x)+1) * (i32.i16 (max_y-min_y)+1) - i32.i64 (length world)

entry part2 s =
  let world = parse s
  let go = true
  let i = 0
  let (_,_,i) =
    loop (world,go,i) while go do
    let world' = step i world
    in if or (map2 (!=) world world')
       then (world', go, i+1)
       else (world', false, i+1)
  in i

-- ==
-- entry: part1
-- input @ data/23.input
-- output { 3757 }

-- ==
-- entry: part2
-- input @ data/23.input
-- output { 918i64 }
