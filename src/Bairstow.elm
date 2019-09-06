module Bairstow exposing (bairstow, divideByQuad)


bairstow : List Float -> Float -> List ( Float, Float ) -> List ( Float, Float )
bairstow coefs epsilon roots =
    case coefs of
        [] ->
            roots

        [ _ ] ->
            roots

        [ a1, a0 ] ->
            if a1 < epsilon then
                roots
            else
                ( a0 / a1, 0 ) :: roots

        [ a2, a1, a0 ] ->
            (quadraticRoots a2 a1 a0 epsilon) |> List.append roots

        an :: an1 :: rest ->
            []


type alias WorkingVariables =
    { a1 : Float
    , a0 : Float
    , b1 : Float
    , b0 : Float
    , f1 : Float
    , f0 : Float
    }


type alias Variables =
    { c : Float
    , d : Float
    , g : Float
    , h : Float
    }


variables : Float -> Float -> List Float -> Float -> Float -> Variables
variables a1 a0 restA u v =
    let
        variables1 =
            restA
                |> List.foldl
                    (\nextA ->
                        \vars ->
                            { a1 = vars.a0
                            , a0 = nextA
                            , b1 = vars.b0
                            , b0 = vars.a1 - u * vars.b0 - v * vars.b1
                            , f1 = vars.f0
                            , f0 = vars.b1 - u * vars.f0 - v * vars.f1
                            }
                    )
                    { a1 = a1, a0 = a0, b1 = 0, b0 = 0, f1 = 0, f0 = 0 }
    in
        { c = 0, d = 0, g = 0, h = 0 }


divideByQuad : List Float -> List Float -> Float -> Float -> List Float
divideByQuad result quotiant u v =
    case quotiant of
        a2 :: a1 :: a0 :: rest ->
            let
                k =
                    a2 / u
            in
                divideByQuad (result ++ [ k ]) (a1 - k * v :: a0 - k :: rest) u v

        _ ->
            result


quadraticRoots : Float -> Float -> Float -> Float -> List ( Float, Float )
quadraticRoots a2 a1 a0 epsilon =
    let
        d =
            a1 ^ 2 - 4 * a2 * a0

        r1 =
            -a1 / (2 * a2)
    in
        if abs d < epsilon then
            [ ( r1, 0 ) ]
        else if d < 0 then
            let
                im =
                    (sqrt (negate d)) / (2 * a2)
            in
                [ ( r1, im ), ( r1, negate im ) ]
        else
            let
                r2 =
                    (sqrt d) / (2 * a2)
            in
                [ ( r1 + r2, 0 ), ( r1 - r2, 0 ) ]
