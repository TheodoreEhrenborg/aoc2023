import «Aoc2023»

def digits : List Char :=
  List.map Nat.digitChar $ List.range 10

def filter_for_digits (str: String) : String :=
    List.asString $ List.filter (digits.contains .) $ String.toList str

def calibValue (line : String) : Option Nat :=
    (line.front.toString.append line.back.toString).toNat?

def sum := Array.foldl (.+.) 0

def solve1 := (sum <$> .) ∘ (Array.mapM id) ∘ Array.map ( calibValue ∘ filter_for_digits)

def main : IO Unit := do
  IO.println $ solve1 (<- IO.FS.lines "data/1/test1.txt")
  IO.println $ solve1 (<- IO.FS.lines "data/1/day1.txt")
