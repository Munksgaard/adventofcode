load "Listsort";
load "Int";

fun possibleTriangle a b c = a + b > c

fun parseNum acc [] = (acc, [])
  | parseNum acc (n :: ns) =
    if #"0" <= n andalso n <= #"9" then
        parseNum (ord n - ord #"0" + acc * 10) ns
    else (acc, n :: ns)

fun parseLine s =
  case (Listsort.sort Int.compare o
        map (#1 o parseNum 0 o explode) o
        String.tokens (not o Char.isDigit)) s of
      [n1, n2, n3] => possibleTriangle n1 n2 n3
    | _ => raise Fail "impossible"

fun foldLine f acc fin =
  case TextIO.inputLine fin of
      NONE => acc
    | SOME s => foldLine f (f (s, acc)) fin

val solution =
    let val fin = TextIO.openIn "3.txt"
        val result =
            foldLine (fn (line, acc) =>
                         if parseLine line then acc + 1 else acc)
                     0 fin
        val () = TextIO.closeIn fin
    in result end

fun groupIn3s [] = []
  | groupIn3s (x1 :: y1 :: z1 ::
               x2 :: y2 :: z2 ::
               x3 :: y3 :: z3 :: xs) = [x1, x2, x3] ::
                                       [y1, y2, y3] ::
                                       [z1, z2, z3] :: groupIn3s xs
  | groupIn3s _ = raise Fail "impossible"

fun possibleTriangle sides =
  case Listsort.sort Int.compare sides of
      [a, b, c] => a + b > c
    | _ => raise Fail "impossible"

val solution2 =
    let val fin = TextIO.openIn "3.txt"
        val tokens = String.tokens (not o Char.isDigit) (TextIO.inputAll fin)
        val nums = map (#1 o parseNum 0 o explode) tokens
        val groups = groupIn3s nums
        val result = foldl (fn (triangle, acc) =>
                               if possibleTriangle triangle then
                                   acc + 1
                               else acc) 0 groups
        val () = TextIO.closeIn fin
    in result end
