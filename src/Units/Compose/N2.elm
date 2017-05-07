module Units.Compose.N2 exposing (..)

import Json.Encode
import Json.Decode
import Math.Vector2
import Lens exposing (Lens)



type alias N2  a =
    { x : a
    , y : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> N2 a
uniform v =
    {  x = v ,  y = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  -> N2 a
from x y =
        { x = x
        , y = y
        }


fromVec2 : Math.Vector2.Vec2 -> N2 Float
fromVec2 input = Math.Vector2.toRecord input

toVec2 : N2 Float -> Math.Vector2.Vec2
toVec2 input = Math.Vector2.fromRecord input


{-| Lens to convert N2 Float from and to Math.Vector2.Vec2`
-}
lensForVec2 : Lens Math.Vector2.Vec2 (N2 Float)
lensForVec2 =
    { name = "Math.Vector2.Vec2 <-> N2"
    , get = \v -> Ok <| fromVec2 v
    , set = \v p -> Ok <| toVec2 v
    }






-- AXIS : Axis
--------------------------------------------------------------------------------


type Axis
        = X
        | Y
        
-- Axis naming metadata

axisNames : List String
axisNames  = [ "x", "y" ]

axisName : Axis -> String
axisName a =
    case a of
        X -> "x"
        Y -> "y"
        

-- Getters / Setters

get : Axis -> N2 a -> a
get axis d =
    case axis of
        X -> d.x
        Y -> d.y
        
set : Axis -> a -> N2 a -> N2 a
set axis v d =
    case axis of
        X -> { d | x = v }
        Y -> { d | y = v }
        
-- Individual fields

{-| Gets the `x` component from `d`
-}
x : N2 v -> v
x d = d.x

{-| Gets the `y` component from `d`
-}
y : N2 v -> v
y d = d.y


-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : Axis -> Lens (N2 b) b
axisToLens a =
    case a of
        X ->
            { name = "x"
            , get = \v -> Ok <| v.x
            , set = \v p -> Ok <| { p | x = v }
            }
        Y ->
            { name = "y"
            , get = \v -> Ok <| v.y
            , set = \v p -> Ok <| { p | y = v }
            }


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatAxisToLens : Axis -> Lens a (N2 b) -> Lens a b
concatAxisToLens axis lens =
    Lens.concat lens <| axisToLens axis



-- MAP -------------------------------------------------------------------------

{-| -arity version of map.
-}
map : (v -> out) -> N2 v  -> N2 out
map f v  =
    { x = f v.x
    , y = f v.y
    }


{-| 2-arity version of map.
-}
map2 : (v -> v2 -> out) -> N2 v  -> N2 v2  -> N2 out
map2 f v  v2  =
    { x = f v.x v2.x
    , y = f v.y v2.y
    }


{-| 3-arity version of map.
-}
map3 : (v -> v2 -> v3 -> out) -> N2 v  -> N2 v2  -> N2 v3  -> N2 out
map3 f v  v2  v3  =
    { x = f v.x v2.x v3.x
    , y = f v.y v2.y v3.y
    }


{-| 4-arity version of map.
-}
map4 : (v -> v2 -> v3 -> v4 -> out) -> N2 v  -> N2 v2  -> N2 v3  -> N2 v4  -> N2 out
map4 f v  v2  v3  v4  =
    { x = f v.x v2.x v3.x v4.x
    , y = f v.y v2.y v3.y v4.y
    }


-- APPLY -----------------------------------------------------------------------

apply : N2 (v -> out) -> N2 v  -> N2 out
apply fns v  =
    { x = fns.x v.x
    , y = fns.y v.y
    }


apply2 : N2 (v -> v2 -> out) -> N2 v  -> N2 v2  -> N2 out
apply2 fns v  v2  =
    { x = fns.x v.x v2.x
    , y = fns.y v.y v2.y
    }


apply3 : N2 (v -> v2 -> v3 -> out) -> N2 v  -> N2 v2  -> N2 v3  -> N2 out
apply3 fns v  v2  v3  =
    { x = fns.x v.x v2.x v3.x
    , y = fns.y v.y v2.y v3.y
    }


apply4 : N2 (v -> v2 -> v3 -> v4 -> out) -> N2 v  -> N2 v2  -> N2 v3  -> N2 v4  -> N2 out
apply4 fns v  v2  v3  v4  =
    { x = fns.x v.x v2.x v3.x v4.x
    , y = fns.y v.y v2.y v3.y v4.y
    }


-- MAP WITH AXIS ---------------------------------------------------------------

mapWithAxis : (Axis -> v -> c) -> N2 v -> N2 c
mapWithAxis f d =
        { x = f X d.x
        , y = f Y d.y
        }

-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> N2 v -> a
fold fn a d =
    a |> fn d.x |> fn d.y 



-- LIST ------------------------------------------------------------------------


toList : N2 v -> List v
toList d =
    [ d.x, d.y ]



fromList : List v ->  Maybe (N2 v)
fromList l =
    case l of
        [ x, y ] -> Just <| N2 x y
        _ -> Nothing





-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
appendUniform : (v -> v -> v) -> N2 v -> N2 v -> N2 v
appendUniform fn a b =
        { x =  fn a.x b.x
        , y =  fn a.y b.y
        }






-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concat : N2 (v -> v -> v) -> N2 v -> List (N2 v) -> N2 v
concat fns empty xs =
    List.foldl (apply2 fns) empty xs

{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> N2 v -> List (N2 v) -> N2 v
concatUniform fn empty xs =
    concat (uniform fn) empty xs



-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> N2 v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("x",  vfn b.x)
        , ("y",  vfn b.y)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (N2 v)
decode vdecoder =
    Json.Decode.map2
        N2
        (Json.Decode.field "x" vdecoder)
        (Json.Decode.field "y" vdecoder)
