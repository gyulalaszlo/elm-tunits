module Units.Compose.Bounds exposing (..)

import Json.Encode
import Json.Decode

type alias Bounds  a =
    { min : a
    , max : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> Bounds a
uniform v =
    {  min = v ,  max = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  -> Bounds a
from min max =
        { min = min
        , max = max
        }




--------------------------------------------------------------------------------

-- Axis

type Limit
        = Min
        | Max
        
-- Axis naming metadata

limitNames : List String
limitNames  = [ "min", "max" ]

limitName : Limit -> String
limitName a =
    case a of
        Min -> "min"
        Max -> "max"
        

-- Getters / Setters

get : Limit -> Bounds a -> a
get axis d =
    case axis of
        Min -> d.min
        Max -> d.max
        
set : Limit -> a -> Bounds a -> Bounds a
set axis v d =
    case axis of
        Min -> { d | min = v }
        Max -> { d | max = v }
        

-- MAP -------------------------------------------------------------------------

map : (v -> c) -> Bounds v -> Bounds c
map f d =
        { min = f d.min
        , max = f d.max
        }

mapWithLimit : (Limit -> v -> c) -> Bounds v -> Bounds c
mapWithLimit f d =
        { min = f Min d.min
        , max = f Max d.max
        }

apply : Bounds (a -> b) -> Bounds a -> Bounds b
apply fns d =
        { min = fns.min d.min
        , max = fns.max d.max
        }

-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> Bounds v -> a
fold fn a d =
    a |> fn d.min |> fn d.max 



-- LIST ------------------------------------------------------------------------


toList : Bounds v -> List v
toList d =
    [ d.min, d.max ]



fromList : List v ->  Maybe (Bounds v)
fromList l =
    case l of
        [ min, max ] -> Just <| Bounds min max
        _ -> Nothing






-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> Bounds v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("min",  vfn b.min)
        , ("max",  vfn b.max)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (Bounds v)
decode vdecoder =
    Json.Decode.map2
        Bounds
        (Json.Decode.field "min" vdecoder)
        (Json.Decode.field "max" vdecoder)
