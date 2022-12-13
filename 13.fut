-- Futhark is extremely unsuited for this.  I tried to come up with an
-- allocation-free way of writing the comparison, but all I did was
-- waste a bunch of hours.  This is supremely inelegant and I am not
-- proud of it at all.
--
-- (Obv. it also crashed the compiler along the way.)

import "utils"
import "lib/github.com/diku-dk/sorts/merge_sort"

def testinput = "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]\n"

def cmp [n][m] (a: string[n]) (b: string[m]): bool =
  let (ok, _, _, _) =
    loop (ok, done, a: string[], b: string[]) = (true,false,a,b) while !done do
    let end r = (r,true,[],[])
    in if length a == 0
       then (true,true,a,b)
       else if is_digit a[0] && is_digit b[0] then
            let x = atoi a
            let y = atoi b
            in if x < y then end true
               else if y < x then end false
               else let (_,a) = span isnt_digit a
                    let (_,b) = span isnt_digit b
                    in (ok,false,a,b)
       else match (a[0], b[0])
            case ('[', '[') -> (ok,false,drop 1 a,drop 1 b)
            case (']', ']') -> (ok,false,drop 1 a,drop 1 b)
            case (']', _) -> end true
            case (_, ']') -> end false
            case ('[', _) -> let (x,y) = span isnt_digit b
                             in (ok,false,a,"[" ++ x ++ "]" ++ y)
            case (_, '[') -> let (x,y) = span isnt_digit a
                             in (ok,false,"[" ++ x ++ "]" ++ y, b)
            case _ -> (ok,false,drop 1 a,drop 1 b)
  in ok

entry part1 s =
  let (get,ls) = lines.lines s
  let n = (length ls + 3 - 1) / 3
  let relevant = tabulate n (\i -> (ls[i * 3], ls[i*3+1]))
  let ok (a,b) = cmp (get a) (get b)
  in relevant
     |> (id &&& indices)
     |> uncurry zip
     |> filter (ok <-< (.0))
     |> map (.1)
     |> map (+1)
     |> i64.sum

def streq [n][m] (a: string[n]) (b: string[m]): bool =
  n == m && all (uncurry (==)) (zip (exactly m a) b)

entry part2 s =
  let (get,ls) = lines.lines (s ++ "\n[[2]]\n[[6]]\n")
  let n = (length ls + 3 - 1) / 3
  let relevant = tabulate n (\i -> [ls[i * 3], ls[i*3+1]]) |> flatten
  let sorted = relevant |> merge_sort (\a b -> cmp (get a) (get b))
  in match (find (\(_,l) -> streq (get l) "[[2]]") (zip (indices sorted) sorted),
            find (\(_,l) -> streq (get l) "[[6]]") (zip (indices sorted) sorted))
     case (#some (a,_), #some (b,_)) -> (a+1)*(b+1)
     case _ -> -1

-- ==
-- entry: part1
-- input @ data/13.in
-- output { 5808i64 }

-- ==
-- entry: part2
-- input @ data/13.in
-- output { 22713i64 }
