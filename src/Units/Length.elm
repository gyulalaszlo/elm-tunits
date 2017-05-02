module Units.Length exposing (..)

{-| Describe me please...
-}

import List.Extra


{-| Various units for a single metric dimension
-}
type Length
    = Meters Float


meters =
    Meters


{-| get the length unit name
-}
lengthUnitString : Length -> String
lengthUnitString l =
    case l of
        Meters _ ->
            "m"


lengthToGL : Length -> Float
lengthToGL l =
    case l of
        Meters m ->
            m


lengthToString : Length -> String
lengthToString l =
    case l of
        Meters m ->
            floatToString 3 m --++ " m"


lengthFromString : String -> Result String Length
lengthFromString s =
    String.toFloat s
        --        |> Result.andThen (\v -> if v < 0 then Err "Negative length given" else Ok)
        |>
            Result.map Meters


inMeters : Length -> Float
inMeters l =
    case l of
        Meters m ->
            m


mapMeters : (Float -> Float) -> Length -> Length
mapMeters fn =
    meters << fn << inMeters


floatToString : Int -> Float -> String
floatToString decimals num =
    if decimals < 1 then
        toString num
    else
        let
            scale =
                (10 ^ decimals)

            str =
                toString <| round <| num * toFloat scale
        in
            String.dropRight decimals str ++ "." ++ String.right decimals str

linearTraits =
    { toString = lengthToString
    , fromString = lengthFromString
    , unitName = lengthUnitString
    , stepUp = (\vv -> Ok <| mapMeters (\v -> v + 0.1) vv)
    , stepDown = (\vv -> Ok <| mapMeters (\v -> v - 0.1) vv)
    }
