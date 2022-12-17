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

def eval_step [n] (flow: [n]i32) (adj: [n][n]i32)
                  (value: i32) (here: i32) (remaining: i32)
                  (there: i32) =
  let t = adj[here,there] + 1
  let remaining = remaining - t
  in (value + remaining * flow[there],
      there,
      remaining)

type path = {visited:u64,remaining:i32,value:i32,here:i32}

def invalid (p: path) = p.remaining < 0

def next_node (visited: u64) (j: i64) : i32 =
  (loop (i,j) = (0,j) while u64.get_bit i visited != 0 || j > 0 do
     (i+1,if u64.get_bit i visited==0 then j-1 else j))
  |> (.0)

def grow [n] (flow: [n]i32) (adj: [n][n]i32) (i: i64) (p: path) : []path =
  tabulate (n-i) (\j ->
                    let next = next_node p.visited j
                    let (value,there,remaining) =
                      eval_step flow adj p.value p.here p.remaining next
                    in {visited=u64.set_bit next p.visited 1,
                        remaining,
                        value,
                        here=there})

def evolve [n] (flow: [n]i32) (adj: [n][n]i32) (ps: []path) : []path =
  (loop (done,front,i) = ([],ps,0) while i < n do
   let n' = n-i
   let new = filter (invalid >-> not)
                    (flatten (map (\p -> grow flow adj i p |> exactly n') front))
   in (done ++ front, new, i+1))
  |> (\(done,front,_) -> done ++ front)

#[noinline]
def process s =
  let (start,vs) = parse s
  let [n] (flow : [n]i32,adj : [n][n]i32) =
    adjacency vs |> find_paths |> shrink start (map (.flow) vs)
  let start = if start == 0 then 0 else i32.i64 (n-1)
  in (start, flow, adj)

#[noinline]
def solve start flow adj remaining =
  let res = evolve flow adj [{visited=0,
                              here=start,
                              remaining,
                              value=0}]
  in res

entry part1 s =
  let (start,flow,adj) = process s
  let res = solve start flow adj 30
  in map (.value) res |> i32.maximum

entry part2 s =
  let (start,flow,adj) = process s
  let p1s = solve start flow adj 26
  let p2s = solve start flow adj 26
  let compatible p1 p2 = (p1.visited & p2.visited) == 0
  let best_of p1 =
    i32.maximum (map (\p2 -> if compatible p1 p2 then p1.value + p2.value else 0) p2s)
  in map best_of p1s |> i32.maximum

-- ==
-- entry: part1
-- input @ data/16.input
-- output { 2265i32 }

-- ==
-- entry: part2
-- input @ data/16.input
-- output { 2811i32 }
