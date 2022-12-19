import "utils"

def testinput = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.\n"

type blueprint = {ore:i32,clay:i32,obsidian:(i32,i32),geode:(i32,i32)}

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
            in {ore=atoi a, clay=atoi b, obsidian=(atoi c,atoi d), geode=(atoi e,atoi f)}
  in map f ls

type res = {ore:i32,clay:i32,obsidian:i32,geode:i32}

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

def res_lte (a: res) (b: res) =
  a.ore <= b.ore &&
  a.clay <= b.clay &&
  a.obsidian <= b.obsidian &&
  a.geode <= b.geode

type plan = {prod:res,stash:res}

def ore_cost (b: blueprint) : res = empty_res with ore = b.ore

def clay_cost (b: blueprint) : res = empty_res with ore = b.clay

def obsidian_cost (b: blueprint) : res = empty_res with ore = b.obsidian.0
                                                   with clay = b.obsidian.1

def geode_cost (b: blueprint) : res = empty_res with ore = b.geode.0
                                                with obsidian = b.geode.1

def max_ore_cost (b: blueprint) =
  b.ore `i32.max` b.clay `i32.max` b.obsidian.0 `i32.max` b.geode.0

def grow (b: blueprint) (p: plan) : [5](bool,plan) =
  let build_ore = ore_cost b `res_lte` p.stash
  let build_clay = clay_cost b `res_lte` p.stash
  let build_obsidian = obsidian_cost b `res_lte` p.stash
  let build_geode = geode_cost b `res_lte` p.stash
  in [(!build_geode && !(build_ore&&build_clay&&build_obsidian&&build_geode),
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

def evolve (b: blueprint) (ps: []plan) : []plan =
  let t = 24 in
  loop ps for _i < t do
    exactly (trace (length ps)) ps
    |> map (\p -> grow b p)
    |> flatten
    |> filter (.0)
    |> map (.1)

entry part1 s =
  let bs = parse s
  let best b =
    evolve b [{prod=empty_res with ore = 1,stash=empty_res}]
    |> map (.stash.geode) |> i32.maximum
  in map best bs
