module Aberth exposing (solve)

import Complex exposing (Complex, add, complex)


type alias Ruffini =
    { p : Complex
    , q : Complex
    }


diff : List Complex -> List Complex -> List Complex
diff =
    List.map2 (\ai -> \bi -> Complex.subtract ai bi)


complexAbs : Complex -> Float
complexAbs x =
    (Complex.toPolar x).abs


norm : List Complex -> Float
norm =
    List.foldl (\xi -> \summand -> summand + complexAbs xi) 0 >> sqrt


horner : List Float -> Complex -> Complex
horner coefs x =
    coefs |> List.foldl (\ci -> \result -> Complex.real ci |> add (Complex.multiply x result)) Complex.zero


hornerRuffini : List Float -> Complex -> Ruffini
hornerRuffini coefs x =
    coefs |> List.foldl (\ci -> \{ p, q } -> { q = Complex.multiply q x |> add p, p = Complex.real ci |> add (Complex.multiply p x) }) { p = Complex.zero, q = Complex.zero }


aberthehrlich : List Float -> List Complex -> Int -> Complex -> Complex
aberthehrlich coefs r idx root =
    let
        pofz =
            hornerRuffini coefs root

        newt =
            Complex.divide pofz.p pofz.q

        folded =
            r
                |> List.foldl
                    (\ri ->
                        \{ i, betai } ->
                            { i = i + 1
                            , betai =
                                betai
                                    |> add
                                        (if i == idx then
                                            Complex.zero

                                         else
                                            Complex.pow (Complex.subtract root ri) (Complex.real -1)
                                        )
                            }
                    )
                    { i = 0, betai = Complex.zero }

        beta =
            folded.betai
    in
    Complex.subtract root (Complex.divide newt (Complex.subtract (Complex.real 1) (Complex.multiply newt beta)))


converge : List Float -> Float -> List Complex -> List Complex
converge coefs epsilon roots =
    let
        loop =
            converge coefs epsilon

        newRoots =
            roots |> List.indexedMap (\idx -> \root -> aberthehrlich coefs roots idx root)
    in
    if (diff newRoots roots |> norm) < epsilon then
        newRoots

    else
        loop roots


solve : List Float -> Float -> List Complex
solve coefs epsilon =
    case coefs of
        [] ->
            []

        [an] -> if (abs an < epsilon) then [Complex.zero] else []

        an :: rest ->
            if abs an < epsilon then
                solve rest epsilon

            else
                let
                    n =
                        List.length coefs - 1

                    nf =
                        toFloat n

                    img =
                        Complex.imaginary 1

                    a0 =
                        rest |> List.foldl (\ai -> \res -> ai) an

                    radius =
                        abs (a0 / an) ^ (1 / nf)

                    theta =
                        2.0 * pi / nf

                    offset =
                        theta / (nf + 1)

                    initialRoots =
                        List.range 0 (n - 1)
                            |> List.map
                                (\k ->
                                    Complex.real radius |> Complex.multiply (Complex.exp (Complex.multiply img (Complex.real (theta * toFloat k + offset))))
                                )
                in
                converge coefs epsilon initialRoots
