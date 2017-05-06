module Units.Compose.Range exposing (..)

import Json.Encode
import Json.Decode

type alias Range  a =
    { min : a
    , max : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> Range a
uniform v =
    {  min = v ,  max = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  -> Range a
from min max =
        { min = min
        , max = max
        }




--------------------------------------------------------------------------------

-- Axis

type Edge
        = Min
        | Max
        
-- Axis naming metadata

edgeNames : List String
edgeNames  = [ "min", "max" ]

edgeName : Edge -> String
edgeName a =
    case a of
        Min -> "min"
        Max -> "max"
        

-- Getters / Setters

get : Edge -> Range a -> a
get axis d =
    case axis of
        Min -> d.min
        Max -> d.max
        
set : Edge -> a -> Range a -> Range a
set axis v d =
    case axis of
        Min -> { d | min = v }
        Max -> { d | max = v }
        

-- MAP -------------------------------------------------------------------------

map : (v -> c) -> Range v -> Range c
map f d =
        { min = f d.min
        , max = f d.max
        }

mapWithEdge : (Edge -> v -> c) -> Range v -> Range c
mapWithEdge f d =
        { min = f Min d.min
        , max = f Max d.max
        }

apply : Range (a -> b) -> Range a -> Range b
apply fns d =
        { min = fns.min d.min
        , max = fns.max d.max
        }

{-| Helper for applying a function for two arguments (like fold)
-}
apply2 : Range (a -> b -> c) -> Range a -> Range b -> Range c
apply2 fns a b =
        { min = fns.min a.min b.min
        , max = fns.max a.max b.max
        }
-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> Range v -> a
fold fn a d =
    a |> fn d.min |> fn d.max 



-- LIST ------------------------------------------------------------------------


toList : Range v -> List v
toList d =
    [ d.min, d.max ]



fromList : List v ->  Maybe (Range v)
fromList l =
    case l of
        [ min, max ] -> Just <| Range min max
        _ -> Nothing






-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> Range v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("min",  vfn b.min)
        , ("max",  vfn b.max)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (Range v)
decode vdecoder =
    Json.Decode.map2
        Range
        (Json.Decode.field "min" vdecoder)
        (Json.Decode.field "max" vdecoder)



{-| Concatenates `a` and `b` using the supplied concatenator function
for all Edge
-}
appendUniform : (v -> v -> v) -> Range v -> Range v -> Range v
appendUniform fn a b =
        { min =  fn a.min b.min
        , max =  fn a.max b.max
        }

-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concat : Range (v -> v -> v) -> Range v -> List (Range v) -> Range v
concat fns empty xs =
    List.foldl (apply2 fns) empty xs

{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> Range v -> List (Range v) -> Range v
concatUniform fn empty xs =
    concat (uniform fn) empty xs
