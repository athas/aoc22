-- Because of how limited this virtual machine is, we can actually
-- compute its intermediate states with a scan.

import "utils"

type op = #addx i32 | #noop

def parse (s: string[]) : []op =
  let (get,ls) = lines.lines s
  let f l = let l' = get l
            in if l'[0] == 'a'
               then #addx (atoi(drop 5 l'))
               else #noop
  in map f ls

type state = (i32,i32)
def initial_state: state = (0,1)

-- Really annoying that the start register value is 1!  That means the
-- initial machine state is not a neutral element for the composition
-- of machine states.  We try to make it work anyway, by always having
-- an additional 1 in the register value; taking care to adjust when
-- combining machine states.

def state_combine (c1,r1) (c2,r2) : state = (c1+c2,r1+r2-1)

def exec (o: op) : state =
  match o case #addx x -> (2, x+1)
          case #noop -> (1, 1)

def cmp `on` get = \x y -> cmp (get x) (get y)

def run s = parse s |> map exec |> scan state_combine initial_state

def find_state [n] (states: [n]state) x =
  let i = binsearch ((<) `on` (.0)) states (x,0)
  in if i < 0
     then initial_state.1
     else states[i `i64.min` (n-1)].1

entry part1 s =
  let states = run s
  let interesting = [20, 60, 100, 140, 180, 220]
  in map (find_state states) interesting |> map2 (*) interesting |> i32.sum

entry part2 s =
  let states = run s
  let close x y = i32.abs (x-y) < 2
  let f i = let i' = i32.i64 i
            in close (i'%40) (find_state states (i'+1))
  let char b = if b then '#' else '.'
  let nl s = s ++ "\n"
  in tabulate 240 f |> map char |> sized (6*40) |> unflatten |> map nl |> flatten

-- ==
-- entry: part1
-- input @ data/10.input
-- output { 15680 }

-- Not bothering testing part 2.
-- ==
-- entry: part2
-- input @ data/10.input
