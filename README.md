# Advent of [Futhark](https://futhark-lang.org)

I also [did this in
2018](https://github.com/athas/advent_of_code_2018), but then Snektron
upstaged me by doing a [much better solution in
2021](https://github.com/Snektron/aoc21), particularly by also doing
the input processing in Futhark.

Following his lead, I'll also try to do it all in Futhark!  And I will
try to abuse the type system along the way.

## Running

We still need a preprocessor to convert text files to Futhark input
data:

```
$ futhark c 1.fut
$ cat 1.input | ./txt2fut.py | ./1 -e part1
```
