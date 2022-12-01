import "utils"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"

-- Turns empty lines into zeroes
def parse (s: string[]): []i32 =
  let (get,ls) = lines.lines s
  in map (\l -> atoi (get l)) ls

entry part1 (s: string[]) =
  parse s
  |> (map (==0) &&& id)
  |> uncurry (segmented_reduce (+) 0)
  |> i32.maximum

entry part2 (s: string[]) =
  parse s
  |> (map (==0) &&& id)
  |> uncurry (segmented_reduce (+) 0)
  |> radix_sort i32.num_bits i32.get_bit
  |> reverse
  |> take 3
  |> foldl (+) 0

-- ==
-- entry: part1
-- input @ data/1.input
-- output { 68802 }

-- ==
-- entry: part2
-- input @ data/1.input
-- output { 205370 }
