import "lib/github.com/diku-dk/segmented/segmented"

type char = u8
type string [n] = [n]char

def dtoi (c: u8): i32 = i32.u8 c - '0'

def atoi [n] (s: string[n]): i32 =
  let (sign,s) = if n > 0 && s[0] == '-' then (-1,drop 1 s) else (1,s)
  in sign * foldl (\s c -> s * 10 + dtoi c) 0 s

def f &&& g = \x -> (f x, g x)

module words : {
  type word [p]
  val words [n] : [n]char -> ?[p].(word [p] -> ?[m].[m]char, ?[k].[k](word [p]))
} = {
  def is_space (x: char) = x == ' '
  def isnt_space x = !(is_space x)

  type word [p] = ([p](), i64, i64)

  def words [n] (s: [n]char) =
    (\(_, i, k) -> #[unsafe] s[i:i+k],
     segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
     |> (id &&& rotate 1)
     |> uncurry zip
     |> zip (indices s)
     |> filter (\(_,(x,y)) -> x > y)
     |> map (\(i,(x,_)) -> ([],i-x+1,x)))
}

module lines : {
  type line [p]
  val lines [n] : [n]char -> ?[p].(line [p] -> ?[m].[m]char, ?[k].[k](line [p]))
} = {
  def is_word (x: char) = x == '\n'
  def isnt_word x = !(is_word x)

  type line [p] = ([p](), i64, i64)

  -- Assumes last line is terminated by newline.
  def lines [n] (s: [n]char) =
    (\(_, i, k) -> #[unsafe] s[i:i+k],
     map (\prev -> prev == '\n') (rotate (-1) s)
     |> zip (indices s)
     |> filter (.1)
     |> map (.0)
     |> (id &&& rotate 1)
     |> uncurry zip
     |> map (\(i, j) -> ([],i, if j < i then n-i-1 else j-i-1)))
}

def count 'a (p: a -> bool) (xs: []a): i32 =
  xs |> map p |> map i32.bool |> i32.sum

def span p xs =
  split (loop i = 0 while i < length xs && !p xs[i] do i + 1) xs
