-- A bit repetitive, but very straightforward and trivial parallelism.

import "utils"

def parse [n] (s: string[n]) =
  resize ((n/4)*4) s |> unflatten |> map (\l -> (l[0], l[2] - 'X' + 'A'))

def shape_score (s: u8) : i32 =
  match s case 'A' -> 1
          case 'B' -> 2
          case 'C' -> 3
          case _ -> 0

def score (c: u8, r: u8) =
  shape_score r +
  match (c,r)
  case ('A', 'B') -> 6
  case ('A', 'C') -> 0
  case ('B', 'C') -> 6
  case ('B', 'A') -> 0
  case ('C', 'A') -> 6
  case ('C', 'B') -> 0
  case _ -> 3

def counter (c: u8, o: u8) =
  (c,
   match (c,o)
   case ('A', 'A') -> 'C'
   case ('A', 'C') -> 'B'
   case ('B', 'A') -> 'A'
   case ('B', 'C') -> 'C'
   case ('C', 'A') -> 'B'
   case ('C', 'C') -> 'A'
   case _ -> c)

entry part1 s = s |> parse |> map score |> i32.sum
entry part2 s = s |> parse |> map counter |> map score |> i32.sum

-- ==
-- entry: part1
-- input @ data/2.input
-- output { 12156 }

-- ==
-- entry: part2
-- input @ data/2.input
-- output { 10835 }
