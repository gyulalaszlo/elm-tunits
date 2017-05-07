module Units.Linear.Scale exposing (..)

import Json.Encode
import Json.Decode




-- TYPE ------------------------------------------------------------------------


{-| Linear unit for
-}
type Scale
    = Identity 
    | Scalar Float
    | Percent Float
-- CONSTRUCTORS


{-| Create a new value
-}
identity : Scale
identity = Identity



{-| Create a new value
-}
scalar : Float -> Scale
scalar = Scalar



{-| Create a new value
-}
percent : Float -> Scale
percent = Percent


-- CONVERSIONS

{-| Converts `v` to its normalized value
-}
normalize : Scale -> Float
normalize v =
    case v of
        Identity  -> 1
        Scalar  v0 -> v0
        Percent  v0 -> v0 / 100.0



{-| Converts `v` to be in 
-}
toIdentity : Scale -> Scale
toIdentity v = Identity




{-| Converts `v` to be in 
-}
toScalar : Scale -> Scale
toScalar v = 
    let input = normalize v
    in case v of
        Identity ->
            let input = 1
            in Scalar  (input)

        Scalar v0 ->
            let input = v0
            in Scalar  (input)

        Percent v0 ->
            let input = v0 / 100.0
            in Scalar  (input)


{-| Converts `v` to be in 
-}
inScalar : Scale -> (Float)
inScalar v =
    case v of
        Identity ->
            let input = 1
            in (input)

        Scalar v0 ->
            let input = v0
            in (input)

        Percent v0 ->
            let input = v0 / 100.0
            in (input)





{-| Converts `v` to be in 
-}
toPercent : Scale -> Scale
toPercent v = 
    let input = normalize v
    in case v of
        Identity ->
            let input = 1
            in Percent  (input * 100.0)

        Scalar v0 ->
            let input = v0
            in Percent  (input * 100.0)

        Percent v0 ->
            let input = v0 / 100.0
            in Percent  (input * 100.0)


{-| Converts `v` to be in 
-}
inPercent : Scale -> (Float)
inPercent v =
    case v of
        Identity ->
            let input = 1
            in (input * 100.0)

        Scalar v0 ->
            let input = v0
            in (input * 100.0)

        Percent v0 ->
            let input = v0 / 100.0
            in (input * 100.0)



