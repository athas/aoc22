import "utils"

def digit (d: u8) : i64 =
  match d case '2' -> 2
          case '1' -> 1
          case '0' -> 0
          case '-' -> -1
          case '=' -> -2
          case _ -> 0

def snafu_to_int (s: string[]) : i64 =
  loop acc = 0 for i < length s do (acc * 5 + digit s[i])

def enum (x: i64) : u8 =
  match x case 0 -> '0'
          case 1 -> '1'
          case 2 -> '2'
          case 3 -> '='
          case 4 -> '-'
          case _ -> ' '

def int_to_snafu (x: i64) : string[] =
  let d i = enum (iterate (i32.i64 i) ((+2) >-> (/5)) x % 5)
  in tabulate 30 d |> reverse |> span (!= '0') |> (.1)

entry part1 s =
  let (get,ls) = lines.lines s
  in ls
     |> map (\l -> snafu_to_int (get l))
     |> i64.sum
     |> int_to_snafu


-- ==
-- entry: part1
-- input @ data/25.input
-- output { [50u8, 61u8, 49u8, 50u8, 45u8, 49u8, 48u8, 48u8, 45u8, 45u8, 49u8, 48u8, 49u8, 50u8, 45u8, 48u8, 61u8, 48u8, 49u8, 50u8] }
