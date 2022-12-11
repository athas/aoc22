-- This one is Death, but not as much as I immediately thought.  The
-- simulation can be parallelised to some extent, although it is not
-- work efficient.  Ths most painful part is the excessive parsing,
-- which even exposed a compiler bug in my original overly complicated
-- version.
--
-- I wasted a lot of time on stupid things, like not realising that
-- the intermediate results might not fit in 32-bit integers.  That is
-- unusual for an AOC problem.

import "utils"
import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"

let testinput = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1\n"

def num_items [n] (s: string[n]) : i64 =
  (loop (x,i) = (0,0) while i < n do
   let j = i + index_of_first (is_digit >-> not) s[i:]
   in (x+1, j + 2)).0

def get_item [n] (i: i64) (s: string[n]) : i32 =
  let (_, j) =
    loop (i,j) = (i,0) while i > 0 do
      (if s[j] == ' ' then i-1 else i, j + 1)
  in atoi s[j:]

type test = #divisible_by i32 i32 i32

def divisor (t: test) =
  match t case #divisible_by d _ _ -> d

def do_test (t: test) (x: i64) : i32 =
  match t case #divisible_by d t f -> if x % i64.i32 d == 0 then t else f

type op = #add i32 | #mul i32 | #square

def do_op (op: op) (x: i64) : i64 =
  match op case #add y -> x + i64.i32 y
           case #mul y -> x * i64.i32 y
           case #square -> x * x

#[noinline]
def parse (s: string[])
: ?[num_items][num_monkeys].([num_monkeys]test,[num_monkeys]op,[num_items]i64,[num_items]i32) =
  let (get_l,ls) = lines.lines s
  let num_monkeys = (length ls + 7 - 1) / 7
  let monkey_lines i = ls[i*7:]
  let num_items' i = let f_ls = monkey_lines i
                     in num_items (drop 18 (get_l f_ls[1]))
  let get_worry i j = let f_ls = monkey_lines i
                      in i64.i32 (get_item j (drop 18 (get_l f_ls[1])))
  let get_test i : test = let f_ls = monkey_lines i
                          let d = atoi (drop 21 (get_l f_ls[3]))
                          let t = atoi (drop 29 (get_l f_ls[4]))
                          let f = atoi (drop 30 (get_l f_ls[5]))
                          in #divisible_by d t f
  let get_op i = let f_ls = monkey_lines i
                 let s = drop 23 (get_l f_ls[2])
                 in if s[0] == '*' then if s[2] == 'o' then #square
                                        else #mul (atoi s[2:])
                    else #add (atoi s[2:])
  let [num_items] (worry: [num_items]i64) = expand num_items' get_worry (iota num_monkeys)
  let monkey = map i32.i64 (replicated_iota (map num_items' (iota num_monkeys)))
  let test = map get_test (iota num_monkeys)
  let op = map get_op (iota num_monkeys)
  in (test, op, worry, monkey :> [num_items]i32)

def turn [num_monkeys][num_items]
         (relax: i64 -> i64)
         (test: [num_monkeys]test) (op: [num_monkeys]op)
         (i: i32)
         (worry: [num_items]i64) (monkey: [num_items]i32) :
         ([num_items]i64, [num_items]i32, i32) =
  let inspected = count (==i) monkey
  let move w m =
    if m != i then (w,m)
     else let w = relax (do_op op[m] w)
          in (w, do_test test[m] w)
  let (worry, monkey) = unzip (map2 move worry monkey)
  in (worry, monkey, inspected)

def round [num_monkeys][num_items]
         (relax: i64 -> i64)
         (test: [num_monkeys]test) (op: [num_monkeys]op)
         (inspections: [num_monkeys]i32, worry: [num_items]i64, monkey: [num_items]i32) :
         ([num_monkeys]i32, [num_items]i64, [num_items]i32) =
  let inspections = copy inspections
  in loop (inspections,worry,monkey) for i < num_monkeys do
     let (worry, monkey, inspected) = turn relax test op (i32.i64 i) worry monkey
     in (inspections with [i] = inspections[i] + inspected, worry, monkey)

def two_largest xs =
  xs
  |> radix_sort i32.num_bits i32.get_bit
  |> reverse
  |> take 2

entry part1 s =
  let (test, op, worry, monkey) = parse s
  in iterate 20 (round (/3) test op) (map (const 0) test, worry, monkey)
     |> (.0)
     |> two_largest
     |> i32.product

-- ==
-- entry: part1
-- input @ data/11.input
-- output { 54036 }

entry part2 s =
  let (test, op, worry, monkey) = parse s
  let d = i64.i32 (i32.product (map divisor test))
  in iterate 10000 (round (%d) test op) (map (const 0) test, worry, monkey)
     |> (.0)
     |> radix_sort i32.num_bits i32.get_bit
     |> map i64.i32
     |> reverse
     |> take 2
     |> i64.product
