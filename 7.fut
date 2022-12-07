-- Oh no, this is very inappropriate for Futhark.
--
-- Many thanks to Aaron Hsu whose PhD thesis contains nice expositions
-- of tree processing in array languages:
-- https://scholarworks.iu.edu/dspace/handle/2022/24749

import "utils"

let testinput =
  "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n"

type op = #ls | #cd | #dotdot | #dir | #file i32

def parse_line (s: string[]) : op =
  if s[0] == '$'
  then if s[2] == 'l' then #ls else if s[5] == '.' then #dotdot else #cd
  else if s[0] == 'd' then #dir else #file (atoi (span (not <-< is_digit) s).0)

#[noinline]
def dir_sizes (s: string[]) =
  let (get,ls) = lines.lines s
  let ops = map (\l -> parse_line (get l)) ls
  let deepen op = match op case #cd -> 1
                           case #dotdot -> -1
                           case _ -> 0
  let interesting (op: op) = match op case #dotdot -> false
                                      case #ls -> false
                                      case #dir -> false
                                      case _ -> true
  let (ops,depths) = zip ops (exscan (+) 0 (map deepen ops))
                     |> filter ((.0) >-> interesting)
                     |> unzip
  let find_parent i d =
    loop i while i > 0 && depths[i] >= d do i - 1
  let max_depth = i32.maximum depths
  let parents = map2 find_parent (indices depths) depths
  let file_size op = match op case #file k -> k
                              case _ -> 0
  let weights =
    loop acc = map file_size ops for d in reverse (1...max_depth) do
       reduce_by_index (copy acc) (+) 0i32
                       (map2 (\di p -> if di == d then p else -1) depths parents)
                       acc
  let is_dir op = match op case #cd -> true
                           case _ -> false
  in zip ops weights |> filter ((.0) >-> is_dir) |> map (.1)

entry part1 (s: string[]) =
  dir_sizes s
  |> drop 1
  |> filter (<= 100000)
  |> i32.sum

entry part2 (s: string[]) =
  let dirs = dir_sizes s
  let available = 70000000
  let unused = trace (available - dirs[0])
  let need = 30000000
  let to_obtain = need-unused
  in dirs |> filter (>=to_obtain) |> i32.minimum

-- ==
-- entry: part1
-- input @ data/7.input
-- output { 1306611 }
