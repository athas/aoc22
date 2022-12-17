-- I did not enjoy this one at all.  I am considering giving up on
-- AOC.  I really wish these problems were calibrated to be solvable
-- without too much time investment; I'm just looking for fun little
-- tasks to do every day in December!  I have enough real challenges
-- at my job.

import "utils"

type valve = {flow: i32, tunnels: [5]i64}

def far: i32 = 1000000

def adjacency [n] (vs: [n]valve): [n][n]i32 =
  let on_node i v = spread n far v.tunnels (map (const 1) v.tunnels) with [i] = 0
  in map2 on_node (indices vs) vs

def update [n] (adj: [n][n]i32) =
  tabulate_2d n n (\i j -> i32.minimum (map2 (+) adj[i] adj[:,j]))

def find_paths [n] (adj: [n][n]i32) =
  (.0) <|
  loop (adj,go) = (adj,true) while go do
  let adj' = update adj
  in (adj', map2 (map2 (!=)) adj adj' |> flatten |> or)

def shrink [n] (keep: i64) (flow: [n]i32) (adj: [n][n]i32) =
  let keep = map2 (\i f -> i == keep || f > 0) (indices flow) flow
  let n' = i64.i32 (count id keep)
  let shrink' l = zip keep l |> filter (.0) |> map (.1) |> exactly n'
  in (shrink' flow,
      adj |> map shrink' |> shrink')

#[noinline]
def parse s : (i64,[]valve) =
  let (get,ls) = lines.lines s
  let mkcode a b = i32.u8 a*256 + i32.u8 b
  let on_line l =
    let ls = get l
    let code = mkcode ls[6] ls[7]
    let flow = atoi (drop 23 ls)
    let tunnels = if ls[49] == ' ' then drop 50 ls else drop 49 ls
    let mktunnel i = let j = i * 4
                     in if j < length tunnels then mkcode tunnels[j] tunnels[j+1]
                        else -1
    in (code, {flow, tunnels=tabulate 5 mktunnel})
  let (codes, valves) = unzip (map on_line ls)
  let start = zip codes (indices codes)
              |> find ((.0) >-> (==mkcode 'A' 'A'))
              |> from_opt (-1,-1)
              |> (.1)
  let link' x = if x == -1 then -1 else index_of_first (==x) codes
  let link {flow,tunnels} = {flow,tunnels=map link' tunnels}
  in (start,map link valves)

def eval [n] (start: i32) (flow: [n]i32) (adj: [n][n]i32) (steps: []i32) =
  let (sum, _, remaining) =
    loop (sum, here, remaining) = (0,start,30) for there in steps do
    let t = adj[here,there] + 1
    let remaining = remaining - t
    in (sum + remaining * flow[there],
        there,
        remaining)
  in (remaining, sum)

-- The first 'i' nodes of 'nodes' have been visited in the given
-- order, and the rest are in arbitrary order.
type path [n] = {nodes:[n]i32,remaining:i32,value:i32}

def invalid (p: path[]) = p.remaining < 0

def next_node [n] (nodes: *[n]i32) (i: i64) (j: i64) : *[n]i32 =
  let next = nodes[i+j]
  let tmp = nodes[i]
  let nodes[i] = next
  let nodes[i+j] = tmp
  in nodes

def grow [n] (start: i32) (flow: [n]i32) (adj: [n][n]i32) (i: i64) (p: path[n]) : [](path[n]) =
  tabulate (n-i) (\j ->
                    let nodes = next_node (copy p.nodes) i j
                    let (remaining,value) = eval start flow adj (take (i+1) nodes)
                    in {nodes,remaining,value})

def evolve [n] (start: i32) (flow: [n]i32) (adj: [n][n]i32) (ps: [](path[n])) : [](path[n]) =
  (loop (ps,i) = (ps,0) while i < n do
   let n' = n-trace i
   in (ps ++
          filter (invalid >-> not)
          (flatten (map (\p -> grow start flow adj i p |> exactly n') ps)),
       i+1))
  |> (.0)

entry part1 s =
  let (start,vs) = parse s
  let [n] (flow : [n]i32,adj : [n][n]i32) = adjacency vs |> find_paths |> shrink start (map (.flow) vs)
  let start = i32.i64 (n-1)
  let (remaining,value) = eval start flow adj []
  let res = evolve start flow adj [{nodes=map i32.i64 (iota n),remaining,value}]
  let best = map (.value) res |> i32.maximum
  let best_path = from_opt res[0] (find (\p -> p.value == best) res)
  in best_path.value

-- ==
-- entry: part1
-- input @ data/16.input
-- output { 2265i32 }
