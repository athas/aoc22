-- Not parallel and not nice.  I am very inexperienced at programming
-- with coordinate space transformations, so I really hacked this one
-- up.

import "utils"

type pos = {x:i64,y:i64}
type dir = #n|#w|#e|#s
type world [n][m] = {cells: [n][m]u8, xbounds: [n]((pos,dir),(pos,dir)), ybounds: [m]((pos,dir),(pos,dir))}

type move = #l | #r | #go i64

#[noinline]
def parse s: (world[][], []u8) =
  let (get,ls) = lines.lines s
  let linelen l = length (get l)
  let (map_lines,ls) = span (\l -> linelen l == 0) ls
  let m = map linelen map_lines |> i64.maximum
  let on_line y l = (pad_to m ' ' (get l),
                     (({x=linelen l-1,y},#w), ({x=index_of_first (!=' ') (get l),y},#e)))
  let (cells,xbounds) = unzip (imap on_line map_lines)
  let movs = get ls[1]
  let find_ybounds x l = (({x,y=length cells - index_of_first (!=' ') (reverse l) - 1},#n),
                          ({x,y=index_of_first (!=' ') l},#s))
  let ybounds = imap find_ybounds (transpose cells)
  in ({cells, xbounds, ybounds}, movs)

def get_move (s: []u8) : (move, []u8) =
  match s[0] case 'L' -> (#l, s[1:])
             case 'R' -> (#r, s[1:])
             case _ -> (#go (i64.i32 (atoi s)), (span isnt_digit s).1)

type me = {pos: pos, dir: dir}

def progress [n][m] (w: world[n][m]) ({dir=dir,pos={x,y}}: me) : me =
  match dir case #n -> if y-1 < 0 || w.cells[y-1,x] == ' '
                       then {pos=(w.ybounds[x].0.0), dir=w.ybounds[x].0.1}
                       else {pos={y=y-1,x}, dir}
            case #s -> if y+1 >= n || w.cells[y+1,x] == ' '
                       then {pos=(w.ybounds[x].1.0), dir=w.ybounds[x].1.1}
                       else {pos={y=y+1,x}, dir}
            case #w -> if x-1 < 0 || w.cells[y, x-1] == ' '
                       then {pos=w.xbounds[y].0.0, dir=w.xbounds[y].0.1}
                       else {pos={y,x=x-1}, dir}
            case #e -> if x+1 >= m || w.cells[y, x+1] == ' '
                       then {pos=w.xbounds[y].1.0, dir=w.xbounds[y].1.1}
                       else {pos={y,x=x+1}, dir}

def move [n][m] (world: world [n][m]) (move: move) (me: me) : me =
  match move case #l -> (match me.dir case #n -> me with dir = #w
                                      case #w -> me with dir = #s
                                      case #s -> me with dir = #e
                                      case #e -> me with dir = #n)
             case #r -> (match me.dir case #n -> me with dir = #e
                                      case #w -> me with dir = #n
                                      case #s -> me with dir = #w
                                      case #e -> me with dir = #s)
             case #go n -> loop me for _i < n do
                           let me' = progress world me
                           in if world.cells[me'.pos.y,me'.pos.x] == '#'
                              then me
                              else me'

def dir_num (d: dir) : i64 =
  match d case #e -> 0
          case #s -> 1
          case #w -> 2
          case #n -> 3

def walk world (steps: []u8) =
  let me = {pos=({y=0,x=index_of_first (!=' ') world.cells[0]}),dir=#e}
  let (_,_,me) =
    loop (world,steps,me) while length steps > 0 do
    let (mv, steps) = get_move steps
    in (world, steps, move world mv me)
  in ((me.pos.y+1) * 1000 + (me.pos.x+1) * 4 + dir_num me.dir)

entry part1 s =
  let (world,steps) = parse s
  in walk world steps

-- Note: does not work for example input, only real input.
def fold [n][m] (world: world[n][m]) : world[n][m] =
  let k = n/4
  let on_xbound y =
    if y < k then (({y=3*k-1-y%k,x=0},#e),
                   ({y=3*k-1-y%k,x=2*k-1},#w))
    else if y < 2*k then (({y=k*2,x=y%k},#s),
                          ({y=k-1,x=k*2+y%k},#n))
    else if y < 3*k then (({y=k-1-y%k,x=k},#e),
                          ({y=k-1-y%k,x=k*3-1},#w))
    else (({y=0,x=k+y%k},#s),
          ({y=3*k-1,x=k+y%k},#n))
  let on_ybound x =
    if x < k then (({x=k,y=k+x%k}, #e),
                   ({x=2*k+x%k,y=0}, #s))
    else if x < 2*k then (({x=0,y=3*k+x%k}, #e),
                          ({x=k-1,y=3*k+x%k}, #w))
    else (({y=4*k-1, x=x%k}, #n),
          ({x=k*2-1, y=k+x%k}, #w))
  in world with xbounds = map on_xbound (indices world.xbounds)
           with ybounds = map on_ybound (indices world.ybounds)

entry part2 s =
  let (world, steps) = parse s
  in walk (fold world) steps

-- ==
-- entry: part1
-- input @ data/22.input
-- output { 117102i64 }

-- ==
-- entry: part2
-- input @ data/22.input
-- output { 135297i64 }
