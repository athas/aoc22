-- This is the one; the most sequential one of them all.  Even the
-- input has no parallelism to exploit!

import "utils"

def testinput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n"

def width (piece: i64) : i64 =
  match piece
  case 0 -> 4
  case 1 -> 3
  case 2 -> 3
  case 3 -> 1
  case _ -> 2

def height (piece: i64) : i64 =
  match piece
  case 0 -> 1
  case 1 -> 3
  case 2 -> 3
  case 3 -> 4
  case _ -> 2

def space [h] (piece: i64) ((x,y):(i64,i64)) (world: [h][7]char) =
  x >= 0 && x <= 7 - width piece && y - height piece + 1 >= 0 &&
  match piece
  case 0 ->
    world[h-1-y,x+0] + world[h-1-y,x+1] + world[h-1-y,x+2] + world[h-1-y,x+3] == 0
  case 1 ->
    world[h-1-(y-0),x+1] + world[h-1-(y-1),x+0] + world[h-1-(y-1),x+1] + world[h-1-(y-1),x+2] + world[h-1-(y-2),x+1] == 0
  case 2 ->
    world[h-1-(y-0),x+2] + world[h-1-(y-1),x+2] + world[h-1-(y-2),x+0] + world[h-1-(y-2),x+1] + world[h-1-(y-2),x+2] == 0
  case 3 ->
    world[h-1-(y-0),x] + world[h-1-(y-1),x] + world[h-1-(y-2),x] + world[h-1-(y-3),x] == 0
  case 4 ->
    world[h-1-(y-0),x+0] + world[h-1-(y-1),x+0] + world[h-1-(y-0),x+1] + world[h-1-(y-1),x+1] == 0
  case _ -> assert false false

def push piece (x,y) (dir: u8) world =
  let x' = if dir == '<' then x-1 else x+1
  in if space piece (x',y) world then (x',y) else (x,y)

def fall piece (x,y) world : opt (i64,i64) =
  if space piece (x,y-1) world then #some (x,y-1) else #none

def put_piece [h] (piece: i64) ((x,y):(i64,i64)) (world: *[h][7]char) =
  match piece
  case 0 -> world with [h-1-y,x+0] = 1
                  with [h-1-y,x+1] = 1
                  with [h-1-y,x+2] = 1
                  with [h-1-y,x+3] = 1
  case 1 -> world with [h-1-(y-0),x+1] = 1
                  with [h-1-(y-1),x+0] = 1
                  with [h-1-(y-1),x+1] = 1
                  with [h-1-(y-1),x+2] = 1
                  with [h-1-(y-2),x+1] = 1
  case 2 -> world with [h-1-(y-0),x+2] = 1
                  with [h-1-(y-1),x+2] = 1
                  with [h-1-(y-2),x+0] = 1
                  with [h-1-(y-2),x+1] = 1
                  with [h-1-(y-2),x+2] = 1
  case 3 -> world with [h-1-(y-0),x] = 1
                  with [h-1-(y-1),x] = 1
                  with [h-1-(y-2),x] = 1
                  with [h-1-(y-3),x] = 1
  case 4 -> world with [h-1-(y-0),x+0] = 1
                  with [h-1-(y-1),x+0] = 1
                  with [h-1-(y-0),x+1] = 1
                  with [h-1-(y-1),x+1] = 1
  case _ -> assert false world

type cycle = ()
def no_cycle: cycle = ()

def place [h] [j]
              (J: [j]u8)
              (T: *[h][7]u8) (jet: i64) (piece: i64) (maxy: i64) :
              (*[h][7]u8, i64, i64, i64) =
  let pos = (2, maxy + 3 + height piece)
  let (_, pos, jet) =
    loop (go,pos,jet) = (true,pos,jet) while go do
    let pos = push piece pos J[(jet%j)] T
    in match fall piece pos T
       case #some pos -> (true,pos,jet+1)
       case #none -> (false,pos,jet+1)
  in (put_piece piece pos T, jet, (piece + 1)%5, i64.max maxy pos.1)

def solve [j] (J: [j]u8) (count: i64) =
  let h = 20000
  let c = 200
  let T = replicate h (replicate 7 0)
  let cycles = replicate c no_cycle
  let (jet, maxy, piece, additional) = (0,-1,0,0)
  let (T, _, _, maxy, _) =
    loop (T, jet, piece, maxy, count) while count > 0 do
    let (T, jet, piece, maxy) = place J T jet piece maxy
    let count = count - 1
    in (T, jet, piece, maxy, count)
  in maxy + additional + 1

entry part1 s = solve (init s) 2022

-- ==
-- entry: part1
-- input @ data/17.input
-- output { 3130i64 }
