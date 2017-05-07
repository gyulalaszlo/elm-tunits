module Units.Linear.Length exposing (..)

import Json.Encode
import Json.Decode




-- TYPE ------------------------------------------------------------------------


{-| Linear unit for
-}
type Length
    = Zero 
    | Meters Float
    | Millimeters Float
    | HumanScale Float Float
-- CONSTRUCTORS


{-| Create a new value
-}
zero : Length
zero = Zero



{-| Create a new value
-}
meters : Float -> Length
meters = Meters



{-| Create a new value
-}
millimeters : Float -> Length
millimeters = Millimeters



{-| Create a new value
-}
humanScale : Float -> Float -> Length
humanScale = HumanScale


-- CONVERSIONS

{-| Converts `v` to its normalized value
-}
normalize : Length -> Float
normalize v =
    case v of
        Zero  -> 0
        Meters  v0 -> v0
        Millimeters  v0 -> v0 * 0.001
        HumanScale  v0 v1 -> v0 * 1000 + v1 * 0.0001



{-| Converts `v` to be in 
-}
toZero : Length -> Length
toZero v = Zero




{-| Converts `v` to be in 
-}
toMeters : Length -> Length
toMeters v = 
    let input = normalize v
    in case v of
        Zero ->
            let input = 0
            in Meters  (input)

        Meters v0 ->
            let input = v0
            in Meters  (input)

        Millimeters v0 ->
            let input = v0 * 0.001
            in Meters  (input)

        HumanScale v0 v1 ->
            let input = v0 * 1000 + v1 * 0.0001
            in Meters  (input)


{-| Converts `v` to be in 
-}
inMeters : Length -> (Float)
inMeters v =
    case v of
        Zero ->
            let input = 0
            in (input)

        Meters v0 ->
            let input = v0
            in (input)

        Millimeters v0 ->
            let input = v0 * 0.001
            in (input)

        HumanScale v0 v1 ->
            let input = v0 * 1000 + v1 * 0.0001
            in (input)





{-| Converts `v` to be in 
-}
toMillimeters : Length -> Length
toMillimeters v = 
    let input = normalize v
    in case v of
        Zero ->
            let input = 0
            in Millimeters  (input * 1000.0)

        Meters v0 ->
            let input = v0
            in Millimeters  (input * 1000.0)

        Millimeters v0 ->
            let input = v0 * 0.001
            in Millimeters  (input * 1000.0)

        HumanScale v0 v1 ->
            let input = v0 * 1000 + v1 * 0.0001
            in Millimeters  (input * 1000.0)


{-| Converts `v` to be in 
-}
inMillimeters : Length -> (Float)
inMillimeters v =
    case v of
        Zero ->
            let input = 0
            in (input * 1000.0)

        Meters v0 ->
            let input = v0
            in (input * 1000.0)

        Millimeters v0 ->
            let input = v0 * 0.001
            in (input * 1000.0)

        HumanScale v0 v1 ->
            let input = v0 * 1000 + v1 * 0.0001
            in (input * 1000.0)





{-| Converts `v` to be in 
-}
toHumanScale : Length -> Length
toHumanScale v = 
    let input = normalize v
    in case v of
        Zero ->
            let input = 0
            in HumanScale  (input / 1000.0) (toFloat (truncate (input * 1000.0)))

        Meters v0 ->
            let input = v0
            in HumanScale  (input / 1000.0) (toFloat (truncate (input * 1000.0)))

        Millimeters v0 ->
            let input = v0 * 0.001
            in HumanScale  (input / 1000.0) (toFloat (truncate (input * 1000.0)))

        HumanScale v0 v1 ->
            let input = v0 * 1000 + v1 * 0.0001
            in HumanScale  (input / 1000.0) (toFloat (truncate (input * 1000.0)))


{-| Converts `v` to be in 
-}
inHumanScale : Length -> (Float, Float)
inHumanScale v =
    case v of
        Zero ->
            let input = 0
            in (input / 1000.0, toFloat (truncate (input * 1000.0)))

        Meters v0 ->
            let input = v0
            in (input / 1000.0, toFloat (truncate (input * 1000.0)))

        Millimeters v0 ->
            let input = v0 * 0.001
            in (input / 1000.0, toFloat (truncate (input * 1000.0)))

        HumanScale v0 v1 ->
            let input = v0 * 1000 + v1 * 0.0001
            in (input / 1000.0, toFloat (truncate (input * 1000.0)))



