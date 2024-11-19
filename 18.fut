-- Second part is solved with a stencil.  The most efficient solution
-- is probably more like a BFS, but pretty proud I made it this far
-- without turning to inefficient stencils!

import "utils"

type cube = {x:i32,y:i32,z:i32}

def parse (s: string[]) : []cube =
  let (get,ls) = lines.lines s
  let f l = let ls = get l
            let (x,ls) = span (==',') ls
            let (y,ls) = span (==',') (drop 1 ls)
            let z = drop 1 ls
            in {x=atoi x, y=atoi y, z=atoi z}
  in map f ls

def adjacent (c1: cube) (c2: cube) =
  c2 == (c1 with x = c1.x + 1) ||
  c2 == (c1 with x = c1.x - 1) ||
  c2 == (c1 with y = c1.y + 1) ||
  c2 == (c1 with y = c1.y - 1) ||
  c2 == (c1 with z = c1.z + 1) ||
  c2 == (c1 with z = c1.z - 1)

entry part1 s =
  let cubes = parse s
  in map (\c -> 6-count (adjacent c) cubes) cubes |> i32.sum

type cell = #air | #steam | #cube

def evolve [nx][ny][nz] (grid: [nx][ny][nz]cell) =
  let steam (x,y,z) = grid[x,y,z] == #steam
  let f x y z : cell =
    match grid[x,y,z]
    case #air ->
      if steam (x-1,y,z)||steam (x+1,y,z)||
         steam (x,y-1,z)||steam (x,y+1,z)||
         steam (x,y,z-1)||steam (x,y,z+1)
      then #steam
      else #air
    case #cube -> #cube
    case #steam -> #steam
  in tabulate_3d nx ny nz f

def put_cubes [nx][ny][nz] (grid: *[nx][ny][nz]cell) (cubes: []cube) =
  let flat_idx c =
    i64.i32 c.x * (ny*nz) + i64.i32 c.y * nz + i64.i32 c.z
  in scatter (flatten_3d grid) (map flat_idx cubes) (map (const #cube) cubes)
     |> sized (nx*ny*nz) |> unflatten_3d

def find_steam nx ny nz cubes : [nx][ny][nz]bool =
  let grid = tabulate_3d nx ny nz
                         (\x y z -> if x == 0 || y == 0 || z == 0 ||
                                       x == nx-1 || y == ny-1 || z == nz-1
                                    then #steam else #air)
  let grid = put_cubes grid cubes
  let go = true
  let (grid,_) = loop (grid, go) while go do
                 let grid' = evolve grid
                 in (grid', or (flatten_3d (map2 (map2 (map2 (!=))) grid grid')))
  let is_steam x = match x case #steam -> true
                           case _ -> false
  in map (map (map is_steam)) grid

entry part2 s =
  let cubes = parse s
  let nx = map (.x) cubes |> i32.maximum |> (+3) |> i64.i32
  let ny = map (.y) cubes |> i32.maximum |> (+3) |> i64.i32
  let nz = map (.z) cubes |> i32.maximum |> (+3) |> i64.i32
  let steam = find_steam nx ny nz cubes
  let at (x,y,z) = if x < 0 || y < 0 || z < 0 then 1 else i32.bool steam[x,y,z]
  let count_steam c = at(c.x-1,c.y,c.z) + at(c.x+1,c.y,c.z) +
                      at(c.x,c.y-1,c.z) + at(c.x,c.y+1,c.z) +
                      at(c.x,c.y,c.z-1) + at(c.x,c.y,c.z+1)
  in map count_steam cubes |> i32.sum

-- ==
-- entry: part1
-- input @ data/18.input
-- output { 3466 }

-- ==
-- entry: part2
-- input @ data/18.input
-- output { 2012 }
