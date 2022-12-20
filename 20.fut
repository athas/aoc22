-- Oh no, this is very inappropriate for arrays, but I am much too
-- tired to implement a doubly linked list for this.
--
-- I had hoped to do something parallel here, via a composition of
-- permutations, but I'm not sure it would actually work.

import "utils"

def parse (s: string[]) =
  let (get,ls) = lines.lines s
  in map (\l -> i64.i32 (atoi (get l))) ls

def shift [n] 'a (from: i64) (d: i64) (A: [n]a): [n]a =
  if d >= 0 && from+d < n
  then tabulate n (\i -> A[if i == from+d then from
                           else if i < from then i
                           else if i < from+d then i+1
                           else i])
  else if d > 0 && from + d >= n
  then tabulate n (\i -> A[if i < (from+d)%n then i
                           else if i == (from+d+1)%n then from
                           else if i > (from+d)%n && i <= from then i-1
                           else i])
  else if d < 0 && from + d > 0
  then tabulate n (\i -> A[if i == from+d then from
                           else if i > from then i
                           else if i > from+d then i-1
                           else i])
  else let d = n+d-1
       in tabulate n (\i -> A[if i == from+d then from
                              else if i < from then i
                              else if i < from+d then i+1
                              else i])

def move [n] (i: i64) (A: [n](i64,i64)): [n](i64,i64) =
  let j = index_of_first ((.0) >-> (==i)) A
  in shift j (i64.i64 A[j].1 %% (n-1)) A

def moves [n] (A: [n](i64,i64)) =
  loop A = copy A for i < n do move i A

entry part1 (s: string[]) =
  parse s
  |> (indices &&& id)
  |> uncurry zip
  |> moves
  |> map (.1)
  |> (\A -> let j = index_of_first (==0) A
            in A[(j+1000)%length A] + A[(j+2000)%length A] + A[(j+3000)%length A])

entry part2 (s: string[]) =
  parse s
  |> map (*811589153)
  |> (indices &&& id)
  |> uncurry zip
  |> iterate 10 moves
  |> map (.1)
  |> (\A -> let j = index_of_first (==0) A
            in A[(j+1000)%length A] + A[(j+2000)%length A] + A[(j+3000)%length A])

-- ==
-- entry: part1
-- input @ data/20.input
-- output { 1591i64 }

-- ==
-- entry: part2
-- input @ data/20.input
-- output { 14579387544492i64 }
