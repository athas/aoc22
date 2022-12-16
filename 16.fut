import "utils"

def testinput = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II\n"

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

def shrink [n] (flow: [n]i32) (adj: [n][n]i32) =
  let keep = map2 (\i f -> i == 0 || f > 0) (indices flow) flow
  let n' = i64.i32 (count id keep)
  let shrink' l = zip keep l |> filter (.0) |> map (.1) |> exactly n'
  in (shrink' flow,
      adj |> map shrink' |> shrink')

#[noinline]
def parse s : []valve =
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
  let link' x = if x == -1 then -1 else index_of_first (==x) codes
  let link {flow,tunnels} = {flow,tunnels=map link' tunnels}
  in map link valves

def eval [n] (flow: [n]i32) (adj: [n][n]i32) (steps: []i32) =
  let (flow_sum, flow_cur, _, i) =
    loop (flow_sum, flow_cur, here, i) = (0,0,0,1) for there in steps do
    let t = adj[here,there] + 1
    in trace (flow_sum + t * flow_cur,
              flow_cur + flow[there],
              there,
              i+t)
  in flow_sum + flow_cur * (30-i+1)

entry part1 s l =
  let vs = parse s
  let (flow,adj) = adjacency vs |> find_paths |> shrink (map (.flow) vs)
  in eval flow adj l
