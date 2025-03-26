(* Good Code: Functional, concise, and uses pattern matching *)
let rec factorial n =
  match n with
  | 0 -> 1
  | n -> n * factorial (n - 1)

(* Bad Code: Imperative, verbose, and uses mutable variables *)
let bad_factorial n =
  let mutable result = 1 in
  let mutable i = 1 in
  while i <= n do
    result <- result * i;
    i <- i + 1
  done;
  result

