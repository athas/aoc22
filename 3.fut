-- Nicely solved by using bitmasks to model the sets, but
-- unfortunately needs irregular parallelism.

import "lib/github.com/diku-dk/segmented/segmented"
import "utils"

def priority (c: u8) =
  if c <= 'Z' then 27 + (c - 'A') else 1 + (c - 'a')

def mask c = 1u64 << u64.u8 (priority c)

entry part1 (s: string[]) =
  let (get,ls) = lines.lines s
  let on_line l =
    let [n] (l: [n]u64) = map mask (get l)
    in u64.ctz (reduce (|) 0 (take (n/2) l) & reduce (|) 0 (drop (n/2) l))
  in map on_line ls |> i32.sum

entry part2 (s: string[]) =
  let (get,ls) = lines.lines s
  let on_group g =
    let (l1,l2,l3) = (get g[0], get g[1], get g[2])
    in u64.ctz (reduce (|) 0 (map mask l1)
                & reduce (|) 0 (map mask l2)
                & reduce (|) 0 (map mask l3))
  in sized ((length ls / 3)*3) ls |> unflatten |> map on_group |> i32.sum

-- ==
-- entry: part1
-- input @ data/3.input
-- output { 7701 }


-- ==
-- entry: part2
-- input @ data/3.input
-- output { 2644 }
