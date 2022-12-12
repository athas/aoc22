-- This is just breadth-first search.  Somewhat clunky because I have
-- to construct a graph.  This could be simplified a lot by exploiting
-- the structure of the input, but I really wanted to take a graph
-- approach.

import "utils"
import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "bfs"

def find_index_of [n][m] (grid: [n][m]char) (c: char) : (i64,i64) =
  tabulate_2d n m (\i j -> ((i,j), grid[i,j] == c))
  |> flatten
  |> find (.1)
  |> from_opt ((-1,-1),true)
  |> (.0)

def movable [n][m] (grid: [n][m]char) (i:i64,j:i64) (x,y) =
  x >= 0 && x < n && y >= 0 && y < m &&
  (grid[i,j] == 'S' || grid[x,y] == 'E' || grid[i,j] + 1 >= grid[x,y])

def node_num_edges [n][m] (grid: [n][m]char) (i,j) =
  i64.bool (movable grid (i,j) (i-1,j))
  + i64.bool (movable grid (i,j) (i+1,j))
  + i64.bool (movable grid (i,j) (i,j-1))
  + i64.bool (movable grid (i,j) (i,j+1))

def to_flat [n][m] (_: [n][m]char) (i,j) = i * m + j

def node_edge [n][m] (grid: [n][m]char) (i, j) (e: i64) =
  -- Very gross; really wish I came up with a way to write this as a
  -- closed form.
  let ps = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
  in to_flat grid (filter (movable grid (i,j)) ps)[e]

#[noinline]
def parse_grid (s: string[]) =
  -- Special case the sizes so I don't have to get out my line
  -- splitter.
  let [n][m] (grid: [n][m]char) =
    copy (if length s == 45
          then s |> unflatten 5 9 |> map (take 8)
          else s |> unflatten 41 182 |> map (take 181))
  let start = find_index_of grid 'S'
  let end = find_index_of grid 'E'
  let grid[start.0,start.1] = 'a'
  let grid[end.0,end.1] = 'z'
  in (start, end, grid)

#[noinline]
def parse1 (s: string[]) : ([]u8,i64,i64,graph[][]) =
  let [n][m] (start, end, grid: [n][m]char) = parse_grid s
  let coords = tabulate_2d n m (\i j -> (i,j)) |> flatten
  let nodes_n_edges = map (node_num_edges grid) coords
  let nodes_start_index = exscan (+) 0 nodes_n_edges
  let edges_dest = expand (node_num_edges grid) (node_edge grid) coords
  in (flatten grid |> matches coords,
      to_flat grid start,
      to_flat grid end,
      {nodes_n_edges,
       nodes_start_index,
       edges_dest})

entry part1 (s: string[]) =
  let (_,start,end,g) = parse1 s
  let costs = bfs g (map (==start) (node_indices g))
  in costs[end]

entry part2 (s: string[]) =
  let (grid,_,end,g) = parse1 s
  let g' = invert_graph g
  let costs = bfs g' (map (==end) (node_indices g))
  in zip grid costs
     |> filter (\(x,y) -> x == 'a' && y > 0)
     |> map (.1)
     |> i32.minimum

-- ==
-- entry: part1
-- input @ data/12.input
-- output { 528 }

-- ==
-- entry: part2
-- input @ data/12.input
-- output { 522 }
