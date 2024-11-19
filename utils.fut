import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/merge_sort"

type char = u8
type string [n] = [n]char

def dtoi (c: u8): i32 = i32.u8 c - '0'

def is_digit (c: u8) = c >= '0' && c <= '9'

def isnt_digit = not <-< is_digit

def atoi [n] (s: string[n]): i32 =
  let (sign,s) = if n > 0 && s[0] == '-' then (-1,drop 1 s) else (1,s)
  in sign * (loop (acc,i) = (0,0) while i < length s do
               if is_digit s[i]
               then (acc * 10 + dtoi s[i], i+1)
               else (acc, n)).0

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

module splits : {
  type split [p]
  val splits [n] 'a : (a -> bool) -> [n]a -> ?[p].(split [p] -> ?[m].[m]a, ?[k].[k](split [p]))
} = {
  type split [p] = ([p](), i64, i64)

  def splits [n] 'a (p: a -> bool) (s: [n]a) =
    (\(_, i, k) -> #[unsafe] s[i:i+k],
     s
     |> rotate (-1)
     |> map2 (\i x -> (i,i == 0 || p x)) (indices s)
     |> filter (.1)
     |> map (.0)
     |> (id &&& rotate 1)
     |> uncurry zip
     |> map (\(i, j) -> ([],i, if j < i -- Last element is special.
                               then if p (last s) then n-i-1 else n-i
                               else j-i-1)))
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

def index_of_first p xs =
  loop i = 0 while i < length xs && !p xs[i] do i + 1

def span p xs = let i = index_of_first p xs in (take i xs, drop i xs)

def windows k s =
  map (\i -> take k (drop i s)) (take (length s - k) (indices s))

def exscan 'a [n] (op: a -> a -> a) (ne: a) (as: [n]a) : *[n]a =
  scan op ne (map2 (\i a -> if i == 0 then ne else a) (indices as) (rotate (-1) as))

-- Finds smallest element greater than x.
def binsearch [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (_, end) =
    loop (start,end) = (0,n-1) while start <= end do
    let mid = (start+end)/2
    in if xs[mid] `lte` x
       then (mid+1,end)
       else (start,mid-1)
  in end

def exactly [n] 't (m: i64) (arr: [n]t) : [m]t = arr :> [m]t

def matches [n][m] 'a 'b (_: [m]b) (as: [n]a) : [m]a = as :> [m]a

type opt 'a = #some a | #none

def from_opt 'a x (y: opt a) =
  match y case #some y' -> y'
          case #none -> x

def find 'a (p: a->bool) (xs:[]a) : opt a =
  let op a b =
    match (a,b)
    case (#none, b) -> b
    case (a, #none) -> a
    case _ -> a
  in xs
     |> map (\x -> if p x then #some x else #none)
     |> reduce op #none

def pad_to k x xs = sized k (xs ++ replicate (k - length xs) x)

def argmin 'a (lte: a -> a -> bool) (as: []a) : i64 =
  let cmp i j = match (i,j)
                case (-1, _) -> j
                case (_, -1) -> i
                case _ -> if as[i] `lte` as[j] then i else j
  in reduce cmp (-1) (indices as)

def dedup eq lte ps =
  let ps = merge_sort lte ps
  let ok i x y = (i == 0 || not (eq x y),x)
  in map3 ok (indices ps) ps (rotate 1 ps)
     |> filter (.0)
     |> map (.1)

def imap f xs = map2 f (indices xs) xs

def indices_2d [n][m] 't (_: [n][m]t) = tabulate_2d n m (\i j -> (i,j))

def hist_2d 'a [k] (op: a -> a -> a) (ne: a) (n: i64) (m: i64) (is: [k](i64,i64)) (as: [k]a) : *[n][m]a =
  reduce_by_index_2d (replicate n (replicate m ne)) op ne is as

