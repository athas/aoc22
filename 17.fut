-- This is the one; the most sequential one of them all.  Even the
-- input has no parallelism to exploit!

import "utils"

def testinput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n"

def rock_width (rock_num: i64) : i64 =
  match rock_num
  case 0 -> 4
  case 1 -> 3
  case 2 -> 3
  case 3 -> 1
  case _ -> 2

def rock_height (rock_num: i64) : i64 =
  match rock_num
  case 0 -> 1
  case 1 -> 3
  case 2 -> 3
  case 3 -> 4
  case _ -> 2

def space_for_rock [h] (rock_num: i64) ((x,y):(i64,i64)) (world: [h][7]char) =
  x >= 0 && x <= 7 - rock_width rock_num && y <= i64.i64 h - rock_height rock_num &&
  match rock_num
  case 0 ->
    world[y,x+0] + world[y,x+1] + world[y,x+2] + world[y,x+3] == 0
  case 1 ->
    world[y+0,x+1] + world[y+1,x+0] + world[y+1,x+1] + world[y+1,x+2] + world[y+2,x+1] == 0
  case 2 ->
    world[y+0,x+2] + world[y+1,x+2] + world[y+2,x+0] + world[y+2,x+1] + world[y+2,x+2] == 0
  case 3 ->
    world[y+0,x] + world[y+1,x] + world[y+2,x] + world[y+3,x] == 0
  case 4 ->
    world[y+0,x+0] + world[y+1,x+0] + world[y+0,x+1] + world[y+1,x+1] == 0
  case _ -> assert false false

def push rock_num (x,y) (dir: u8) world =
  let x' = if dir == '<' then x-1 else x+1
  in if space_for_rock rock_num (x',y) world then (x',y) else (x,y)

def fall rock_num (x,y) world : opt (i64,i64) =
  if space_for_rock rock_num (x,y+1) world then #some (x,y+1) else #none

def put_rock (rock_num: i64) ((x,y):(i64,i64)) (world: *[][7]char) =
  match rock_num
  case 0 -> world with [y,x+0] = 1
                  with [y,x+1] = 1
                  with [y,x+2] = 1
                  with [y,x+3] = 1
  case 1 -> world with [y+0,x+1] = 1
                  with [y+1,x+0] = 1
                  with [y+1,x+1] = 1
                  with [y+1,x+2] = 1
                  with [y+2,x+1] = 1
  case 2 -> world with [y+0,x+2] = 1
                  with [y+1,x+2] = 1
                  with [y+2,x+0] = 1
                  with [y+2,x+1] = 1
                  with [y+2,x+2] = 1
  case 3 -> world with [y+0,x] = 1
                  with [y+1,x] = 1
                  with [y+2,x] = 1
                  with [y+3,x] = 1
  case 4 -> world with [y+0,x+0] = 1
                  with [y+1,x+0] = 1
                  with [y+0,x+1] = 1
                  with [y+1,x+1] = 1
  case _ -> assert false world

def drop_rock [j][h] (jets: string[j])
                     (rock_num: i64)
                     (jet_num: i64)
                     (top: i64)
                     (world: *[h][7]char) =
  let pos = (2,top-3-rock_height rock_num)
  let (_, pos, jet_num) =
    loop (go,pos,jet_num) = (true,pos,jet_num) while go do
    let pos = push rock_num pos jets[(jet_num%j)] world
    in match fall rock_num pos world
       case #some pos -> (true,pos,jet_num+1)
       case #none -> (false,pos,jet_num+1)
  in (jet_num, i64.min top pos.1, put_rock rock_num pos world)

entry part1 (s: string[]) =
  let jets = init s -- Drop linebreak.
  let h = 5000
  let world = replicate h (replicate 7 0)
  let top = i64.i64 (length world)
  let jet_num = 0
  let (_, top, _) =
    loop (jet_num, top, world) for rock_num < 2022 do
      drop_rock jets (rock_num%5) jet_num top world
  let height = h - i64.i64 top
  in height

-- Determine from my input; this sucks.
def first_fixed_point : i64 = 1954
def fixed_point_stride : i64 = 1715
def fixed_point_top_inc : i64 = 2655

entry part2 (s: string[]) =
  let [j] (jets: [j]u8) = init s -- Drop linebreak.
  let h = 2000000
  let world = replicate h (replicate 7 0)
  let skipped = 0
  let top = i64.i64 (length world)
  let jet_num = 0
  let rock_num = 0
  let target = 1000000000000i64
  let (_, skipped, top, _, _) =
    loop (jet_num, skipped, top, world, rock_num) while rock_num < target do
      if (rock_num-(first_fixed_point-fixed_point_stride)) % fixed_point_stride == 0
      then (jet_num,
            skipped+fixed_point_top_inc,
            top,
            world,
            rock_num + fixed_point_stride)
      else
        let (jet_num, top, world) = drop_rock jets (rock_num%5) jet_num top world
        in (jet_num, skipped, top, world, rock_num + 1)
  in skipped + top

-- ==
-- entry: part1
-- input @ data/17.input
-- output { 3130i64 }
