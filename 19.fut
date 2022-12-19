-- BFS with little meaningful pruning.  Not proud of this one.  Takes
-- a while to run and requires a lot of memory.  Memory usage kept
-- down by using 8-bit counters.  Some crude deduplication.
--
-- This one is possibly too slow to run in GitHub Actions.

import "utils"
import "lib/github.com/diku-dk/sorts/merge_sort"

def testinput = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.\n"

type blueprint = {ore:u8,clay:u8,obsidian:(u8,u8),geode:(u8,u8)}

#[noinline]
def parse (s: string[]) : []blueprint =
  let (get,ls) = lines.lines s
  let f l = let ls = drop 20 (get l)
            let (_,ls) = span is_digit ls
            let (a,ls) = span isnt_digit ls
            let (_,ls) = span is_digit ls
            let (b,ls) = span isnt_digit ls
            let (_,ls) = span is_digit ls
            let (c,ls) = span isnt_digit ls
            let (_,ls) = span is_digit ls
            let (d,ls) = span isnt_digit ls
            let (_,ls) = span is_digit ls
            let (e,ls) = span isnt_digit ls
            let (_,ls) = span is_digit ls
            let (f,_) = span isnt_digit ls
            in {ore=u8.i32 (atoi a),
                clay=u8.i32 (atoi b),
                obsidian=(u8.i32(atoi c),u8.i32(atoi d)),
                geode=(u8.i32(atoi e), u8.i32(atoi f))}
  in map f ls

type res = {ore:u8,clay:u8,obsidian:u8,geode:u8}

def empty_res : res = {ore=0,clay=0,obsidian=0,geode=0}

def add_res (a: res) (b: res) =
  {ore=a.ore + b.ore,
   clay = a.clay + b.clay,
   obsidian = a.obsidian + b.obsidian,
   geode = a.geode + b.geode}

def sub_res (a: res) (b: res) =
  {ore=a.ore - b.ore,
   clay = a.clay - b.clay,
   obsidian = a.obsidian - b.obsidian,
   geode = a.geode - b.geode}

type plan = {prod:res,stash:res}

def ore_cost (b: blueprint) : res = empty_res with ore = b.ore

def clay_cost (b: blueprint) : res = empty_res with ore = b.clay

def obsidian_cost (b: blueprint) : res = empty_res with ore = b.obsidian.0
                                                   with clay = b.obsidian.1

def geode_cost (b: blueprint) : res = empty_res with ore = b.geode.0
                                                with obsidian = b.geode.1

def max_ore_cost (b: blueprint) =
  b.ore `u8.max` b.clay `u8.max` b.obsidian.0 `u8.max` b.geode.0

def grow (b: blueprint) (p: plan) : [5](bool,plan) =
  let build_ore = b.ore <= p.stash.ore
  let build_clay = b.clay <= p.stash.ore
  let build_obsidian = b.obsidian.0 <= p.stash.ore && b.obsidian.1 <= p.stash.clay
  let build_geode = b.geode.0 <= p.stash.ore && b.geode.1 <= p.stash.obsidian
  in [(!build_geode && p.stash.ore < 2*max_ore_cost b,
       p with stash = add_res p.stash p.prod),
      (!build_geode && build_ore && p.prod.ore < max_ore_cost b,
       p with stash = (add_res p.stash p.prod `sub_res` ore_cost b)
         with prod = (p.prod with ore = p.prod.ore + 1)),
      (!build_geode && build_clay && p.prod.clay < b.obsidian.1,
       p with stash = (add_res p.stash p.prod `sub_res` clay_cost b)
         with prod = (p.prod with clay = p.prod.clay + 1)),
      (!build_geode && build_obsidian && p.prod.obsidian < b.geode.1,
       p with stash = (add_res p.stash p.prod `sub_res` obsidian_cost b)
         with prod = (p.prod with obsidian = p.prod.obsidian + 1)),
      (build_geode,
       p with stash = (add_res p.stash p.prod `sub_res` geode_cost b)
         with prod = (p.prod with geode = p.prod.geode + 1))]

def prune (ps: []plan) : []plan =
  let g = u8.maximum (map (.prod.geode) ps)
  let min = if g > 0 then g-1 else g
  in filter ((.prod.geode) >-> (>=min)) ps

def res_lt (a: res) (b: res) =
  let word x =
    u32.u8 x.ore | (u32.u8 x.clay<<8) | (u32.u8 x.obsidian<<16) | (u32.u8 x.geode<<24)
  in word a < word b

def res_lte (a: res) (b: res) =
  res_lt a b || a == b

def plan_lte (a: plan) (b: plan): bool =
  if a.prod `res_lt` b.prod
  then true
  else if a.prod == b.prod
  then a.stash `res_lte` b.stash
  else false

def dedup (ps: []plan) : []plan =
  let ps = merge_sort plan_lte ps
  let ok i x y = (i == 0 || x!=y,x)
  in map3 ok (indices ps) ps (rotate 1 ps)
     |> filter (.0)
     |> map (.1)

def evolve (t: i32) (b: blueprint) (ps: []plan) : []plan =
  loop ps for _i < t do
    ps
    |> map (grow b)
    |> flatten
    |> filter (.0)
    |> map (.1)
    |> prune
    |> dedup

entry part1 s =
  let bs = parse s
  let best i b =
    #[sequential]
    evolve 24 b [{prod=empty_res with ore = 1,stash=empty_res}]
    |> map (.stash.geode) |> map i32.u8 |> i32.maximum |> (*i32.i64 (i+1))
  in map2 best (indices bs) bs |> i32.sum

entry part2 s =
  let bs = parse s
  let best b =
    evolve 32 b [{prod=empty_res with ore = 1,stash=empty_res}]
    |> map (.stash.geode) |> map i32.u8 |> i32.maximum
  in map best (take 3 bs) |> foldl (*) 1

-- ==
-- entry: part1
-- input @ data/19.input
-- output { 1616 }

-- ==
-- entry: part2
-- input @ data/19.input
-- output { 8990 }
