-- Only the input parsing is parallel here.  I suspect the search
-- could also be parallelised, at least when falling down, but it is
-- probably not worth it.  This program also uncovered a compiler bug
-- (#1798).

import "utils"
import "lib/github.com/diku-dk/segmented/segmented"

type pos = (i32,i32)

def linesegs (s: string[]) : [](pos,pos) =
  let (get,fs) = splits.splits (=='-') s
  let on_f f = let fs = get f
               let fs = if fs[0] == '>' then drop 2 fs else fs
               let (x,y) = span isnt_digit fs
               in (atoi x, atoi (drop 1 y))
  in map on_f fs |> (id &&& rotate 1) |> uncurry zip |> init

def parse (s: string[]) =
  let (get,ls) = lines.lines s
  let max_segs = 100
  let noseg = ((-1,-1),(-1,-1))
  let get_segs l = pad_to max_segs noseg (linesegs (get l))
  in map get_segs ls |> flatten |> filter (!=noseg)

def air: u8 = '.'
def rock: u8 = '#'
def sand: u8 = 'o'

def drop [rows][cols] (x,y) (world: [rows][cols]u8) =
  let (x,y,_) =
    loop (x,y,continue) = (x,y,true) while continue do
      if x == 0 || x == cols-1 then (cols,y,false)
      else if y == rows-1 then (x,rows,false)
      else if world[y+1,x] == air then (x,y+1,true)
      else if world[y+1,x-1] == air then (x-1,y+1,true)
      else if world[y+1,x+1] == air then (x+1,y+1,true)
      else (x,y,false)
  in (x,y)

def sim [rows][cols] (x,y) (world: [rows][cols]u8) : i32 =
  let (_,i,_) =
    loop (world,i,continue) = (copy world,0,true) while continue do
    let (x,y) = drop (x,y) world
    in if x == cols || y == rows then (world, i, false)
       else (world with [y,x] = sand, i+1, true)
  in i

def mkworld s =
  let linelength ((x1,y1),(x2,y2)) =
    i64.i32 (if x1 == x2
             then i32.abs(y2 - y1)+1
             else i32.abs(x2 - x1)+1)
  let coord ((x1,y1),(x2,y2)) i =
    if x1 == x2
    then (x1,y1+i32.i64 i*(i32.sgn(y2-y1)))
    else (x1+i32.i64 i*(i32.sgn(x2-x1)),y1)
  let lines = parse s
  let max_x = i64.i32 (i32.maximum (map (\((x1,_),(x2,_)) -> i32.max x1 x2) lines))
  let max_y = i64.i32 (i32.maximum (map (\((_,y1),(_,y2)) -> i32.max y1 y2) lines))
  let flat (x,y) = i64.i32 y*(max_x+1)+i64.i32 x
  let rocks = expand linelength coord lines
  in spread ((max_y+1)*(max_x+1)) air (map flat rocks) (map (const rock) rocks)
     |> unflatten

entry part1 s = sim (500,0) (mkworld s)

def sim2 [rows][cols] (x,y) (world: [rows][cols]u8) : i32 =
  let (_,i,_) =
    loop (world,i,continue) = (copy world,0,true) while continue do
    let (x',y') = drop (x,y) world
    in if (x',y') == (x,y) then (world, i+1, false)
       else (world with [y',x'] = sand, i+1, true)
  in i

entry part2 s =
  let [rows][cols] (world: [rows][cols]u8) = mkworld s
  let padding = cols -- Ugly!  But probably enough.
  let cols' = cols + padding*2
  in sim2 (500+padding,0)
          (world
           |> map (\r -> replicate padding air ++ r ++ replicate padding air :> [cols']u8)
           |> (++[replicate cols' air, replicate cols' rock]))

-- ==
-- entry: part1
-- input @ data/14.input
-- output { 892 }

-- ==
-- entry: part2
-- input @ data/14.input
-- output { 27155 }
