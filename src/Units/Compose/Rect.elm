module Units.Compose.Rect exposing (..)

import Json.Encode
import Json.Decode
import Lens exposing (Lens)



type alias Rect  a =
    { left : a
    , right : a
    , top : a
    , bottom : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> Rect a
uniform v =
    {  left = v ,  right = v ,  top = v ,  bottom = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  ->  a  ->  a  -> Rect a
from left right top bottom =
        { left = left
        , right = right
        , top = top
        , bottom = bottom
        }






-- AXIS : Side
--------------------------------------------------------------------------------


type Side
        = Left
        | Right
        | Top
        | Bottom
        
-- Axis naming metadata

sideNames : List String
sideNames  = [ "left", "right", "top", "bottom" ]

sideName : Side -> String
sideName a =
    case a of
        Left -> "left"
        Right -> "right"
        Top -> "top"
        Bottom -> "bottom"
        

-- Getters / Setters

get : Side -> Rect a -> a
get axis d =
    case axis of
        Left -> d.left
        Right -> d.right
        Top -> d.top
        Bottom -> d.bottom
        
set : Side -> a -> Rect a -> Rect a
set axis v d =
    case axis of
        Left -> { d | left = v }
        Right -> { d | right = v }
        Top -> { d | top = v }
        Bottom -> { d | bottom = v }
        
-- Individual fields

{-| Gets the `left` component from `d`
-}
left : Rect v -> v
left d = d.left

{-| Gets the `right` component from `d`
-}
right : Rect v -> v
right d = d.right

{-| Gets the `top` component from `d`
-}
top : Rect v -> v
top d = d.top

{-| Gets the `bottom` component from `d`
-}
bottom : Rect v -> v
bottom d = d.bottom


-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : Side -> Lens (Rect b) b
axisToLens a =
    case a of
        Left ->
            { name = "left"
            , get = \v -> Ok <| v.left
            , set = \v p -> Ok <| { p | left = v }
            }
        Right ->
            { name = "right"
            , get = \v -> Ok <| v.right
            , set = \v p -> Ok <| { p | right = v }
            }
        Top ->
            { name = "top"
            , get = \v -> Ok <| v.top
            , set = \v p -> Ok <| { p | top = v }
            }
        Bottom ->
            { name = "bottom"
            , get = \v -> Ok <| v.bottom
            , set = \v p -> Ok <| { p | bottom = v }
            }


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatSideToLens : Side -> Lens a (Rect b) -> Lens a b
concatSideToLens axis lens =
    Lens.concat lens <| axisToLens axis



-- MAP -------------------------------------------------------------------------

{-| -arity version of map.
-}
map : (v -> out) -> Rect v  -> Rect out
map f v  =
    { left = f v.left
    , right = f v.right
    , top = f v.top
    , bottom = f v.bottom
    }


{-| 2-arity version of map.
-}
map2 : (v -> v2 -> out) -> Rect v  -> Rect v2  -> Rect out
map2 f v  v2  =
    { left = f v.left v2.left
    , right = f v.right v2.right
    , top = f v.top v2.top
    , bottom = f v.bottom v2.bottom
    }


{-| 3-arity version of map.
-}
map3 : (v -> v2 -> v3 -> out) -> Rect v  -> Rect v2  -> Rect v3  -> Rect out
map3 f v  v2  v3  =
    { left = f v.left v2.left v3.left
    , right = f v.right v2.right v3.right
    , top = f v.top v2.top v3.top
    , bottom = f v.bottom v2.bottom v3.bottom
    }


{-| 4-arity version of map.
-}
map4 : (v -> v2 -> v3 -> v4 -> out) -> Rect v  -> Rect v2  -> Rect v3  -> Rect v4  -> Rect out
map4 f v  v2  v3  v4  =
    { left = f v.left v2.left v3.left v4.left
    , right = f v.right v2.right v3.right v4.right
    , top = f v.top v2.top v3.top v4.top
    , bottom = f v.bottom v2.bottom v3.bottom v4.bottom
    }


-- APPLY -----------------------------------------------------------------------

apply : Rect (v -> out) -> Rect v  -> Rect out
apply fns v  =
    { left = fns.left v.left
    , right = fns.right v.right
    , top = fns.top v.top
    , bottom = fns.bottom v.bottom
    }


apply2 : Rect (v -> v2 -> out) -> Rect v  -> Rect v2  -> Rect out
apply2 fns v  v2  =
    { left = fns.left v.left v2.left
    , right = fns.right v.right v2.right
    , top = fns.top v.top v2.top
    , bottom = fns.bottom v.bottom v2.bottom
    }


apply3 : Rect (v -> v2 -> v3 -> out) -> Rect v  -> Rect v2  -> Rect v3  -> Rect out
apply3 fns v  v2  v3  =
    { left = fns.left v.left v2.left v3.left
    , right = fns.right v.right v2.right v3.right
    , top = fns.top v.top v2.top v3.top
    , bottom = fns.bottom v.bottom v2.bottom v3.bottom
    }


apply4 : Rect (v -> v2 -> v3 -> v4 -> out) -> Rect v  -> Rect v2  -> Rect v3  -> Rect v4  -> Rect out
apply4 fns v  v2  v3  v4  =
    { left = fns.left v.left v2.left v3.left v4.left
    , right = fns.right v.right v2.right v3.right v4.right
    , top = fns.top v.top v2.top v3.top v4.top
    , bottom = fns.bottom v.bottom v2.bottom v3.bottom v4.bottom
    }


-- MAP WITH AXIS ---------------------------------------------------------------

{-| Transform a the values in `v` using `fn`
-}
mapWithAxis : (Side -> v -> out) -> Rect v  -> Rect out
mapWithAxis fn v  =
    { left = fn Left v.left
    , right = fn Right v.right
    , top = fn Top v.top
    , bottom = fn Bottom v.bottom
    }


{-| N-arity version of mapWith
-}
mapWithAxis2 : (Side -> v -> v2 -> out) -> Rect v  -> Rect v2  -> Rect out
mapWithAxis2 fn v  v2  =
    { left = fn Left v.left v2.left
    , right = fn Right v.right v2.right
    , top = fn Top v.top v2.top
    , bottom = fn Bottom v.bottom v2.bottom
    }


{-| N-arity version of mapWith
-}
mapWithAxis3 : (Side -> v -> v2 -> v3 -> out) -> Rect v  -> Rect v2  -> Rect v3  -> Rect out
mapWithAxis3 fn v  v2  v3  =
    { left = fn Left v.left v2.left v3.left
    , right = fn Right v.right v2.right v3.right
    , top = fn Top v.top v2.top v3.top
    , bottom = fn Bottom v.bottom v2.bottom v3.bottom
    }


{-| N-arity version of mapWith
-}
mapWithAxis4 : (Side -> v -> v2 -> v3 -> v4 -> out) -> Rect v  -> Rect v2  -> Rect v3  -> Rect v4  -> Rect out
mapWithAxis4 fn v  v2  v3  v4  =
    { left = fn Left v.left v2.left v3.left v4.left
    , right = fn Right v.right v2.right v3.right v4.right
    , top = fn Top v.top v2.top v3.top v4.top
    , bottom = fn Bottom v.bottom v2.bottom v3.bottom v4.bottom
    }

-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> Rect v -> a
fold fn a d =
    a |> fn d.left |> fn d.right |> fn d.top |> fn d.bottom 



-- LIST ------------------------------------------------------------------------


toList : Rect v -> List v
toList d =
    [ d.left, d.right, d.top, d.bottom ]



fromList : List v ->  Maybe (Rect v)
fromList l =
    case l of
        [ left, right, top, bottom ] -> Just <| Rect left right top bottom
        _ -> Nothing





-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Side
-}
appendUniform : (v -> v -> v) -> Rect v -> Rect v -> Rect v
appendUniform fn a b =
    { left = fn a.left b.left
    , right = fn a.right b.right
    , top = fn a.top b.top
    , bottom = fn a.bottom b.bottom
    }





{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
appendScalar : Rect comparable -> Rect comparable -> Rect comparable
appendScalar  a b =
    { left = min a.left b.left
    , right = max a.right b.right
    , top = min a.top b.top
    , bottom = max a.bottom b.bottom
    }

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
scalarAppender : Rect (comparable -> comparable -> comparable)
scalarAppender =
    { left = min
    , right = max
    , top = min
    , bottom = max
    }




-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUsing : Rect (v -> v -> v) -> Rect v -> List (Rect v) -> Rect v
concatUsing fns empty xs =
    List.foldl (apply2 fns) empty xs


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> Rect v -> List (Rect v) -> Rect v
concatUniform fn empty xs =
    concatUsing (uniform fn) empty xs



-- CONCAT SPECIALIZATION FOR:  comparable

{-| Empty value for Scalar specializations
-}
emptyScalar : Rect comparable
emptyScalar =
    { left = 0
    , right = 0
    , top = 0
    , bottom = 0
    }

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
concatScalar : List (Rect comparable) -> Rect comparable
concatScalar xs =
    List.foldl (apply2 scalarAppender) emptyScalar xs






-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> Rect v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("left",  vfn b.left)
        , ("right",  vfn b.right)
        , ("top",  vfn b.top)
        , ("bottom",  vfn b.bottom)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (Rect v)
decode vdecoder =
    Json.Decode.map4
        Rect
        (Json.Decode.field "left" vdecoder)
        (Json.Decode.field "right" vdecoder)
        (Json.Decode.field "top" vdecoder)
        (Json.Decode.field "bottom" vdecoder)
