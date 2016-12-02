fun skip (x :: xs) =
  if x <> #"L" andalso x <> #"R" then skip xs else x :: xs
  | skip _ = []

fun parseNum [] acc = (acc, [])
  | parseNum (n :: ns) acc =
    if #"0" <= n andalso n <= #"9" then
        parseNum ns (ord n - ord #"0" + acc * 10)
    else (acc, n :: ns);

fun rotateRight (1, 0) = (0, ~1)
  | rotateRight (0, ~1) = (~1, 0)
  | rotateRight (~1, 0) = (0, 1)
  | rotateRight _ = (1, 0)

val rotateLeft = rotateRight o rotateRight o rotateRight

fun parse (#"L" :: xs) (x, y) rot =
  let val (n, xs) = parseNum xs 0
      val (xd, yd) = rotateLeft rot
  in parse (skip xs) (x + xd * n, y + yd * n) (xd, yd) end
  | parse (#"R" :: xs) (x, y) rot =
  let val (n, xs) = parseNum xs 0
      val (xd, yd) = rotateRight rot
  in parse (skip xs) (x + xd * n, y + yd * n) (xd, yd) end
  | parse [] acc _ = acc
  | parse _ _ _ = raise Fail "No parse";

fun dist (x, y) = abs x + abs y;

val input = "L2, L3, L3, L4, R1, R2, L3, R3, R3, L1, L3, R2, R3, L3, R4, R3, R3, L1, L4, R4, L2, R5, R1, L5, R1, R3, L5, R2, L2, R2, R1, L1, L3, L3, R4, R5, R4, L1, L189, L2, R2, L5, R5, R45, L3, R4, R77, L1, R1, R194, R2, L5, L3, L2, L1, R5, L3, L3, L5, L5, L5, R2, L1, L2, L3, R2, R5, R4, L2, R3, R5, L2, L2, R3, L3, L2, L1, L3, R5, R4, R3, R2, L1, R2, L5, R4, L5, L4, R4, L2, R5, L3, L2, R4, L1, L2, R2, R3, L2, L5, R1, R1, R3, R4, R1, R2, R4, R5, L3, L5, L3, L3, R5, R4, R1, L3, R1, L3, R3, R3, R3, L1, R3, R4, L5, L3, L1, L5, L4, R4, R1, L4, R3, R3, R5, R4, R3, R3, L1, L2, R1, L4, L4, L3, L4, L3, L5, R2, R4, L2"

val solution1 = dist (parse (explode input) (0, 0) (0, 1));

fun steps _ 0 _ acc = acc
  | steps (x, y) n (xd, yd) acc =
    let val newpos = (x + xd, y + yd)
    in  steps newpos (n - 1) (xd, yd) (newpos :: acc) end


fun parseWithHist (#"L" :: xs) (coords as ((x, y) :: _)) rot =
  let val (n, xs) = parseNum xs 0
      val rot' = rotateLeft rot
  in parseWithHist (skip xs) (steps (x, y) n rot' [] @ coords) rot' end
  | parseWithHist (#"R" :: xs) (coords as ((x, y) :: _)) rot =
  let val (n, xs) = parseNum xs 0
      val rot' = rotateRight rot
  in parseWithHist (skip xs) (steps (x, y) n rot' [] @ coords) rot' end
  | parseWithHist [] acc _ = rev acc
  | parseWithHist _ _ _ = raise Fail "No parse";

infix isIn
fun x isIn (y :: ys) = if x = y then true else x isIn ys
  | _ isIn _ = false

fun firstRepeat [] = raise Fail "No repeat"
  | firstRepeat (x :: xs) = if x isIn xs then x else firstRepeat xs

val solution2 =
    dist (firstRepeat (parseWithHist (explode input) [(0, 0)] (0, 1)))
