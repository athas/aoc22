-- Inefficient solution (touching all nodes all the time), but I
-- managed to use AD for the second part!

import "utils"

def testinput = "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32\n"

type op = #add | #sub | #mul | #div
type monkey = #const f64 | #op op i32 i32

def do_op (op: op) x y : f64 =
  match op case #add -> x + y
           case #sub -> x - y
           case #mul -> x * y
           case #div -> x / y

def name l = (i32.u8 l[0] << 24) | (i32.u8 l[1] << 16) | (i32.u8 l[2] << 8) | (i32.u8 l[3] << 0)

def parse (s: string[]) : (i32,i32,*[]monkey) =
  let (get,ls) = lines.lines s
  let on_line l: (i32,monkey) =
    let l = get l
    in (name l,
        if length l < 11
        then #const (f64.i32 (atoi (drop 6 l)))
        else #op (match l[11]
                  case '+' -> #add
                  case '-' -> #sub
                  case '*' -> #mul
                  case _ -> #div)
                 (name (drop 6 l)) (name (drop 13 l)))
  let (ids,monkeys) = unzip (map on_line ls)
  let idx x = i32.i64 (index_of_first (==x) ids)
  let link m = match m case #const x -> #const x
                       case #op op x y -> #op op (idx x) (idx y)
  in (idx (name "root"),
      idx (name "humn"),
      map link monkeys)

def nonconst (m: monkey) = match m case #const _ -> false
                                   case _ -> true

def eval (ms: []monkey) =
  let on_monkey (m: monkey): monkey =
    match m
    case #const x -> #const x
    case #op op x y ->
      match (ms[x],ms[y])
      case (#const x, #const y) -> #const (do_op op x y)
      case _ -> #op op x y
  in map on_monkey ms

entry part1 s =
  let (root,_humn,ms) = parse s
  in match (iterate_while (any nonconst) eval ms)[root]
     case #const x -> x
     case #op _ _ _ -> assert false 0

-- ==
-- entry: part1
-- input @ data/21.input
-- output { 62386792426088f64 }

def cost (root: i32) (humn: i32) (ms: []monkey) (x: f64) =
  let ms = copy ms with [humn] = #const x
  in match (iterate_while (any nonconst) eval ms)[root]
     case #const x -> x
     case #op _ _ _ -> assert false 0

def newton (tol: f64) (f: f64 -> f64) (x0: f64) =
  let iteration (_, x, i) =
    let (y, dy) = jvp2 f x 1
    let x' = x - y / dy
    in (f64.abs (x - x') < tol, x', i+1)
  let (_, x, steps) = iterate_until (.0) iteration (false, x0, 0i32)
  in (x, steps)

entry part2 s =
  let (root,humn,ms) = parse s
  let ms = match ms[root] case #op _ x y -> ms with [root] = #op #sub x y
                          case _ -> assert false ms
  in let (x, _) = newton 0.002 (cost root humn ms) 100
     in i64.f64 (f64.round x)

-- ==
-- entry: part2
-- input @ data/21.input
-- output { 3876027196185i64 }
