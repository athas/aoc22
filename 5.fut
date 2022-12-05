-- Very sequential and the input parsing is a bad experience and done
-- hackily.  I suspect the movement part can actually be parallelised.

import "utils"

def parse (s: string[]) =
  let header_line_len = 36
  let num_stack_lines = 8
  let stack_lines = take (num_stack_lines * header_line_len) s
  let (get, move_lines) = lines.lines (drop ((num_stack_lines+1) * header_line_len + 1) s)
  let on_stack_line l = tabulate 9 (\i -> l[1+i*4])
  let on_move_line l = let l' = get l
                       let (num,l') = span (==' ') (drop 5 l')
                       let (from,l') = span (==' ') (drop 6 l')
                       let (to,_) = span (==' ') (drop 4 l')
                       in (atoi num, atoi from - 1, atoi to - 1)
  in ((unflatten num_stack_lines header_line_len stack_lines)
      |> map on_stack_line
      |> (replicate 100 (replicate 9 ' ')++)
      |> transpose
      |> map reverse,
      map on_move_line move_lines)

entry part1 s =
  let sim (state: [][]u8, moves) =
    let (counts,state) =
      loop (counts,state) = (map (count (!=' ')) state, copy state)
      for (num,from,to) in moves do
        loop (counts,state) for _i < num do
        let counts_to = counts[to]
        let state[to,counts_to] = state[from,counts[from]-1]
        let counts[to] = counts[to] + 1
        let counts[from] = counts[from] - 1
        in (counts, state)
    in (map2 (\c s -> s[c-1]) counts state)
  in s |> parse |> trace |> sim

entry part2 s =
  let sim (state: [][]u8, moves) =
    let (counts,state) =
      loop (counts,state) = (map (count (!=' ')) state, copy state)
      for (num,from,to) in moves do
        loop
          (counts,state) =
          (counts with [from] = counts[from] - num, state)
        for i < num do
        let counts_to = counts[to]
        let state[to,counts_to] = state[from,counts[from]+i]
        let counts[to] = counts[to] + 1
        in (counts, state)
    in (map2 (\c s -> s[c-1]) counts state)
  in s |> parse |> trace |> sim

-- ==
-- entry: part1
-- input @ data/5.input
-- output { [70u8,67u8,86u8,82u8,76u8,77u8,86u8,81u8,80u8] }

-- ==
-- entry: part2
-- input @ data/5.input
-- output { [82u8,87u8,76u8,87u8,71u8,74u8,71u8,70u8,68u8] }
