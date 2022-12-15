-- This one is a lovely parallel implementation.  Seems a bit slow, so
-- I'm probably missing some algorithmic trick.  Oh well.

import "utils"
import "lib/github.com/diku-dk/segmented/segmented"

def testinput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3\n"

type pos = {x:i32,y:i32}

def manhattan (a:pos) (b:pos) =
  i32.abs (a.x-b.x) + i32.abs(a.y-b.y)

#[noinline]
def parse (s: string[]) =
  let (get_l,ls) = lines.lines s
  let on_line l = let (get_w, ws) = words.words (get_l l)
                  let sensor = {x=atoi (drop 2 (get_w ws[2])),
                                y=atoi (drop 2 (get_w ws[3]))}
                  let beacon = {x=atoi (drop 2 (get_w ws[8])),
                                y=atoi (drop 2 (get_w ws[9]))}
                  in (sensor, beacon)
  in map on_line ls

def distances haystack needle = map (manhattan needle) haystack

def find_closest haystack needle =
  distances haystack needle |> argmin (<=) |> (\i -> (i, haystack[i]))

def num_surface_points r : i32 = r * 4

def surface_point (p: pos) r (i: i64) =
  let m = num_surface_points r
  let i = i32.i64 i
  in {x=(p.x-r) + (i+1)/2,
      y=p.y + if i < m/2
              then ((i+1)/2 * if i % 2 == 1 then -1 else 1)
              else ((m-i)/2 * if i % 2 == 1 then -1 else 1)}

def covered sensors closest p =
  any (\(s,b) -> manhattan s b >= manhattan s p) (zip sensors closest)

def is_beacon beacons p =
  i32.minimum (distances beacons p) == 0

entry part1 s =
  let (sensors,beacons) = unzip (parse s)
  let closest = map (find_closest beacons) sensors
  let min_x = map2 (\a d -> (a.x - manhattan a d.1)) sensors closest |> i32.minimum
  let max_x = map2 (\a d -> (a.x + manhattan a d.1)) sensors closest |> i32.maximum
  let no_beacon p = covered sensors (map (.1) closest) p && not (is_beacon beacons p)
  in iota (i64.i32 (max_x-min_x))
     |> map i32.i64
     |> map (+min_x)
     |> map (\x -> {x,y=2000000})
     |> count no_beacon

entry part2 s =
  let (sensors,beacons) = unzip (parse s)
  let closest = map (find_closest beacons) sensors
  let ok p = p.x >= 0 && p.x <= 4000000 && p.y >= 0 && p.y <= 4000000 &&
             not (covered sensors (map (.1) closest) p) &&
             not (is_beacon beacons p)
  in map2 (\s (_,b) -> (s,manhattan s b)) sensors closest
     |> expand (\(_,r) -> i64.i32(num_surface_points (r+1)))
               (\(s,r) i -> surface_point s (r+1) i)
     |> find ok
     |> from_opt {x= -1, y= -1}
     |> (\{x,y} -> i64.i32 x * 4000000 + i64.i32 y)

-- ==
-- entry: part1
-- input @ data/15.input
-- output { 5125700 }

-- ==
-- entry: part2
-- input @ data/15.input
-- output { 11379394658764i64 }

