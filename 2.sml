(* First part *)

infix splitAt
local
    fun aux x (y :: ys) acc =
      if x = y then rev acc :: aux x ys []
      else aux x ys (y :: acc)
      | aux _ [] acc = [rev acc]
in fun ys splitAt x = aux x ys [] end

val testinput = "ULL\nRRDDD\nLURDL\nUUUUD"

val input = "RLRDDRLLDLRLUDDULLDRUUULDDLRLUDDDLDRRDUDDDLLURDDDLDDDRDURUDRDRRULUUDUDDRRRLRRRRRLRULRLLRULDRUUDRLRRURDDRLRULDLDULLLRULURRUULLRLLDDDDLLDURRUDLDLURDRDRDLUUUDDRDUUDDULLUURRDRLDDULURRRUDLLULULDLLURURUDRRRRUDRLRDLRRLDDRDDLULDLLLURURDUDRRRRUULURLRDULDRLUDRRUDDUULDURUDLDDURRRDLULLUUDRLLDUUDLDRUDDRLLLLLLDUDUDDLRDLRRDRUDDRRRLLRRDLLRLDDURUURRRDDLDUULLDLDLRURDLLLDDRUUDRUDDDDULRLLDUULRUULLLULURRRLLULDLDUDLDLURUDUDULLDLLUUDRRDRLUURURURURDLURUUDLDRLUDDUUDULDULULLLDLDDULLULLDULRRDRULLURRRULLDDDULULURLRDURLLURUDDULLRUDLRURURRDRDUULDRUUDURDURDDLRDUUULDUUDRDURURDRRRURLLDDLLLURURULULUDLRDLDRDRURLRLULRDLU\nUDLDURRULDRDDLDUULUDLDUULUURDDRUDRURRRUDRURLLDDRURLDLRDUUURDLLULURDDUDDDRRRURLLDLDLULRDULRLULDLUUDLLRLDLRUUULDDUURDLDDRRDLURLDUDDRURDRRURDURRRLUULURDDLRDLDRRRLDUDRLRLLRLDDUULDURUUULLLRRRRRRRDRRRDRLUULDLDDLULDRDUDLLUDRRUDRUUDULRLUURDDDDRRUUDLURULLLURDULUURDRDDURULRUDRRDLRDUUUUUDDDRDRDDRUDRDDDRLRUUDRDRDDDLUDRDRLDRDDRULURDRLDRUDUDRUULRLLUDRDRLLLLDUDRRLLURDLLLDRRUDDUDRLRLDUDRLURRUUULURDDRUURRLDRLRRRUUDLULDDDRDLDUUURLLUULDDRRUDLDDRUDUDUURURDDRDULLLLLULRRRDLRRRDDDLURDDDDLUULLLRDDURRRRLURRLDDLRUULULRDRDDDDLDUUUUUUDRRULUUUDD\nUURDRRUDLURRDDDLUDLRDURUDURDLLLLRDLRLRDDRDRDUUULRDLLDLULULRDUDDRRUUDURULDLUDLRDRUDLDDULLLDDRDLLDULLLURLLRDDLDRDULRRDDULRDURLLRUDRLRRLUDURLDRDLDLRLLLURLRRURDLDURDLUDULRDULLLDRDDRDLDRDULUULURDRRRLDRRUULULLDDRRLDLRUURLRUURLURRLLULUUULRLLDDUDDLRLDUURURUDLRDLURRLLURUDLDLLUDDUULUUUDDDURDLRRDDDLDRUDRLRURUUDULDDLUUDDULLDDRRDDRRRUDUDUDLDLURLDRDLLLLDURDURLRLLLUUDLRRRRUDUDDLDLRUURRLRRLUURRLUDUDRRRRRRRLDUDDRUDDLUDLRDDDRLDUULDRDRRDLDRURDLDRULRLRLUDRDLRRUURUUUUDLDUUULLLRRRRRDLRRURDDLLLLUULDLLRULLUDLLDLLUDLRLRRLRURDDRRL\nURDRDLLRDDDLLLDDLURLRURUURRRLUURURDURRLLUDURRLRLDLUURDLULRRDRUDDLULDLDRLDLRLRRLLLDDDUDDDLRURURRLLDRRRURUDLRDDLLDULDDLDRLUUUDRRRULDUULRDDDLRRLLURDDURLULRDUDURRLLDLLRLDUDDRRDDLRLLLDUDRLUURRLLDULRLDLUUUUUDULUDLULUDDUURRURLDLDRRLDLRRUDUDRRDLDUDDLULLDLLRDRURDRDRRLDDDDRDDRLLDDDLLUDRURLURDRRRRRUDDDUDUDDRDUUDRRUDUDRLULDDURULUURUUUURDRULRLRULLDDRRRUULRRRRURUDLDLRDLLDRLURLRUULLURDUDULRRURLRLLRRLLLURULRRRLDDUULLUUULRRDRULUUUUDRDRRDLRURLRLLRLRRRDRDRLDLUURUURULLDLULRRLRRDRULRRLLLDDURULLDLDLDLUUURDLDLUUDULRLLUDDRRDLLDLDLDURLUURRDDRRURDRLUDRLUUUDLDULDLUDRLDUDDLLRUDULLLLLDRRLLUULLUUURRDDUURDLLRDDLRLLU\nLDUDRRDLUUDDRLLUUULURLDUDLUDLRLDRURLULRLLDDLRRUUUDDDDRDULDDUUDLRUULDRULLRDRUDDURLDUUURRUDUDRDRDURRDLURRRDRLDLRRRLLLRLURUURRDLLRDLDDLLRDUDDRDUULRULRRURLUDDUDDDUULLUURDULDULLLLRUUUDDRRRLDDDLDLRRDRDRDLUULRLULDRULDLRDRRUDULUDLLUDUULRDLRRUUDDLLDUDDRULURRLULDLDRRULDDRUUDDLURDLRDRLULRRLURRULDUURDLUDLLDRLDULLULDLLRDRDLLLUDLRULLRLDRDDDLDDDLRULDLULLRUUURRLLDUURRLRLDUUULDUURDURRULULRUUURULLLRULLURDDLDRLLRDULLUDLDRRRLLLLDUULRRLDURDURDULULDUURLDUDRLRURRDLUUULURRUDRUUUDRUR"

fun up (x, 1) = (x, 1)
  | up (x, y) = (x, y + 1)

fun down (x, ~1) = (x, ~1)
  | down (x, y) = (x, y - 1)

fun left (~1, y) = (~1, y)
  | left (x, y) = (x - 1, y)

fun right (1, y) = (1, y)
  | right (x, y) = (x + 1, y)

fun move (#"U", pos) = up pos
  | move (#"D", pos) = down pos
  | move (#"L", pos) = left pos
  | move (#"R", pos) = right pos

fun dial (~1, 1) = 1
  | dial (0, 1) = 2
  | dial (1, 1) = 3
  | dial (~1, 0) = 4
  | dial (0, 0) = 5
  | dial (1, 0) = 6
  | dial (~1, ~1) = 7
  | dial (0, ~1) = 8
  | dial (1, ~1) = 9
  | dial _ = raise Fail "impossible"

fun followInstructions ins =
  let val result =
          foldl (fn (xs, (pos, acc)) =>
                    let val newpos = foldl move pos xs
                    in (newpos, dial newpos :: acc) end)
                ((0,0), [])
                (explode ins splitAt #"\n");
  in rev (#2 result) end

val solution1 = followInstructions input

(* Second part *)

fun tryMove f pos =
  let val (x, y) = f pos
  in if abs x + abs y > 2 then pos else (x, y) end

val up = tryMove (fn (x, y) => (x, y + 1))
val down = tryMove (fn (x, y) => (x, y - 1))
val left = tryMove (fn (x, y) => (x - 1, y))
val right = tryMove (fn (x, y) => (x + 1, y))

fun move (#"U", pos) = up pos
  | move (#"D", pos) = down pos
  | move (#"L", pos) = left pos
  | move (#"R", pos) = right pos

fun dial (0, 2) = #"1"
  | dial (~1, 1) = #"2"
  | dial (0, 1) = #"3"
  | dial (1, 1) = #"4"
  | dial (~2, 0) = #"5"
  | dial (~1, 0) = #"6"
  | dial (0, 0) = #"7"
  | dial (1, 0) = #"8"
  | dial (2, 0) = #"9"
  | dial (~1, ~1) = #"A"
  | dial (0, ~1) = #"B"
  | dial (1, ~1) = #"C"
  | dial (0, ~2) = #"D"
  | dial _ = raise Fail "impossible"

fun followInstructions ins =
  let val result =
          foldl (fn (xs, (pos, acc)) =>
                    let val newpos = foldl move pos xs
                    in (newpos, dial newpos :: acc) end)
                ((~2,0), [])
                (explode ins splitAt #"\n");
  in rev (#2 result) end

val solution2 = followInstructions input
