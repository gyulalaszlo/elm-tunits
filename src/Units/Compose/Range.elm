module Units.Compose.Range exposing (..)

import Json.Encode
import Json.Decode
import Lens exposing (Lens)



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






-- AXIS : Edge
--------------------------------------------------------------------------------


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
        
-- Individual fields

{-| Gets the `min` component from `d`
-}
min : Range v -> v
min d = d.min

{-| Gets the `max` component from `d`
-}
max : Range v -> v
max d = d.max


-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : Edge -> Lens (Range b) b
axisToLens a =
    case a of
        Min ->
            { name = "min"
            , get = \v -> Ok <| v.min
            , set = \v p -> Ok <| { p | min = v }
            }
        Max ->
            { name = "max"
            , get = \v -> Ok <| v.max
            , set = \v p -> Ok <| { p | max = v }
            }


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatEdgeToLens : Edge -> Lens a (Range b) -> Lens a b
concatEdgeToLens axis lens =
    Lens.concat lens <| axisToLens axis



-- MAP -------------------------------------------------------------------------

{-| -arity version of map.
-}
map : (v -> out) -> Range v  -> Range out
map f v  =
    { min = f v.min
    , max = f v.max
    }


{-| 2-arity version of map.
-}
map2 : (v -> v2 -> out) -> Range v  -> Range v2  -> Range out
map2 f v  v2  =
    { min = f v.min v2.min
    , max = f v.max v2.max
    }


{-| 3-arity version of map.
-}
map3 : (v -> v2 -> v3 -> out) -> Range v  -> Range v2  -> Range v3  -> Range out
map3 f v  v2  v3  =
    { min = f v.min v2.min v3.min
    , max = f v.max v2.max v3.max
    }


{-| 4-arity version of map.
-}
map4 : (v -> v2 -> v3 -> v4 -> out) -> Range v  -> Range v2  -> Range v3  -> Range v4  -> Range out
map4 f v  v2  v3  v4  =
    { min = f v.min v2.min v3.min v4.min
    , max = f v.max v2.max v3.max v4.max
    }


-- APPLY -----------------------------------------------------------------------

apply : Range (v -> out) -> Range v  -> Range out
apply fns v  =
    { min = fns.min v.min
    , max = fns.max v.max
    }


apply2 : Range (v -> v2 -> out) -> Range v  -> Range v2  -> Range out
apply2 fns v  v2  =
    { min = fns.min v.min v2.min
    , max = fns.max v.max v2.max
    }


apply3 : Range (v -> v2 -> v3 -> out) -> Range v  -> Range v2  -> Range v3  -> Range out
apply3 fns v  v2  v3  =
    { min = fns.min v.min v2.min v3.min
    , max = fns.max v.max v2.max v3.max
    }


apply4 : Range (v -> v2 -> v3 -> v4 -> out) -> Range v  -> Range v2  -> Range v3  -> Range v4  -> Range out
apply4 fns v  v2  v3  v4  =
    { min = fns.min v.min v2.min v3.min v4.min
    , max = fns.max v.max v2.max v3.max v4.max
    }


-- MAP WITH AXIS ---------------------------------------------------------------

{-| Transform a the values in `v` using `fn`
-}
mapWithAxis : (Edge -> v -> out) -> Range v  -> Range out
mapWithAxis fn v  =
    { min = fn Min v.min
    , max = fn Max v.max
    }


{-| N-arity version of mapWith
-}
mapWithAxis2 : (Edge -> v -> v2 -> out) -> Range v  -> Range v2  -> Range out
mapWithAxis2 fn v  v2  =
    { min = fn Min v.min v2.min
    , max = fn Max v.max v2.max
    }


{-| N-arity version of mapWith
-}
mapWithAxis3 : (Edge -> v -> v2 -> v3 -> out) -> Range v  -> Range v2  -> Range v3  -> Range out
mapWithAxis3 fn v  v2  v3  =
    { min = fn Min v.min v2.min v3.min
    , max = fn Max v.max v2.max v3.max
    }


{-| N-arity version of mapWith
-}
mapWithAxis4 : (Edge -> v -> v2 -> v3 -> v4 -> out) -> Range v  -> Range v2  -> Range v3  -> Range v4  -> Range out
mapWithAxis4 fn v  v2  v3  v4  =
    { min = fn Min v.min v2.min v3.min v4.min
    , max = fn Max v.max v2.max v3.max v4.max
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





-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Edge
-}
appendUniform : (v -> v -> v) -> Range v -> Range v -> Range v
appendUniform fn a b =
    { min = fn a.min b.min
    , max = fn a.max b.max
    }





{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
appendScalar : Range comparable -> Range comparable -> Range comparable
appendScalar  a b =
    { min = Basics.min a.min b.min
    , max = Basics.max a.max b.max
    }

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
scalarAppender : Range (comparable -> comparable -> comparable)
scalarAppender =
    { min = Basics.min
    , max = Basics.max
    }




-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUsing : Range (v -> v -> v) -> Range v -> List (Range v) -> Range v
concatUsing fns empty xs =
    List.foldl (apply2 fns) empty xs


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> Range v -> List (Range v) -> Range v
concatUniform fn empty xs =
    concatUsing (uniform fn) empty xs



-- CONCAT SPECIALIZATION FOR:  comparable

{-| Empty value for Scalar specializations
-}
emptyScalar : Range comparable
emptyScalar =
    { min = 0
    , max = 0
    }

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
concatScalar : List (Range comparable) -> Range comparable
concatScalar xs =
    List.foldl (apply2 scalarAppender) emptyScalar xs






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
