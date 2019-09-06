module Bairstow exposing (bairstow, divideByQuad)

bairstow : List Float -> Float -> List (Float, Float) -> List (Float, Float)
bairstow coefs epsilon roots =
  case coefs of
    [] -> roots
    [ _ ] -> roots
    [ a1, a0 ] -> if a1 < epsilon then roots else (a0 / a1, 0) :: roots
    [a2, a1, a0] -> (quadraticRoots a2 a1 a0 epsilon) |> List.append roots
    _ -> []

divideByQuad : List Float -> List Float -> Float -> Float -> List Float
divideByQuad result quotiant u v =
  case quotiant of
    a2 :: a1 :: a0 :: rest ->
      let
        k = a2 / u
      in
        divideByQuad (result ++ [k]) (a1 - k*v :: a0 - k :: rest) u v
    _ -> result         

quadraticRoots : Float -> Float -> Float -> Float -> List (Float, Float)
quadraticRoots a2 a1 a0 epsilon =
  let
    d = a1^2 - 4*a2*a0
    r1 = -a1/(2*a2)
  in
    if abs d < epsilon then
      [(r1, 0)]
    else if d < 0 then
      let
        im = (sqrt (negate d))/(2*a2)
      in
        [(r1, im), (r1, negate im)]
    else
      let
        r2 = (sqrt d)/(2*a2)
      in
        [(r1+r2,0), (r1-r2, 0)]