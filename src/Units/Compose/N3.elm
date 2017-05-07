module Units.Compose.N3 exposing (..)

import Json.Encode
import Json.Decode
import Math.Vector3
import Lens exposing (Lens)



type alias N3  a =
    { x : a
    , y : a
    , z : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> N3 a
uniform v =
    {  x = v ,  y = v ,  z = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  ->  a  -> N3 a
from x y z =
        { x = x
        , y = y
        , z = z
        }


zero : N3 number
zero  = uniform 0



one : N3 number
one  = uniform 1









fromVec3 : Math.Vector3.Vec3 -> N3 Float
fromVec3 input = Math.Vector3.toRecord input

toVec3 : N3 Float -> Math.Vector3.Vec3
toVec3 input = Math.Vector3.fromRecord input


{-| Lens to convert N3 Float from and to Math.Vector3.Vec3`
-}
lensForVec3 : Lens Math.Vector3.Vec3 (N3 Float)
lensForVec3 =
    { name = "Math.Vector3.Vec3 <-> N3"
    , get = \v -> Ok <| fromVec3 v
    , set = \v p -> Ok <| toVec3 v
    }






-- AXIS : Axis
--------------------------------------------------------------------------------


type Axis
        = X
        | Y
        | Z
        
-- Axis naming metadata

axisNames : List String
axisNames  = [ "x", "y", "z" ]

axisName : Axis -> String
axisName a =
    case a of
        X -> "x"
        Y -> "y"
        Z -> "z"
        

-- Getters / Setters

get : Axis -> N3 a -> a
get axis d =
    case axis of
        X -> d.x
        Y -> d.y
        Z -> d.z
        
set : Axis -> a -> N3 a -> N3 a
set axis v d =
    case axis of
        X -> { d | x = v }
        Y -> { d | y = v }
        Z -> { d | z = v }
        

-- TRANSFORM A SINGLE AXIS
-- the first value always takes precendence in these maps


{-| Map with the left value taking precendence
-}
mapLAxis : Axis -> (v -> v) -> N3 v  -> N3 v
mapLAxis axis f v =
    case axis of 
        X -> { v | x = f v.x }
        Y -> { v | y = f v.y }
        Z -> { v | z = f v.z }

{-| Map with the right value taking precendence
-}
mapRAxis : Axis -> (v -> v) -> N3 v  -> N3 v
mapRAxis axis f v =
    case axis of 
        X -> { v | x = f v.x }
        Y -> { v | y = f v.y }
        Z -> { v | z = f v.z }

{-| Map with the left value taking precendence
-}
mapLAxis2 : Axis -> (v -> v2 -> v) -> N3 v  -> N3 v2  -> N3 v
mapLAxis2 axis f v v2 =
    case axis of 
        X -> { v | x = f v.x v2.x }
        Y -> { v | y = f v.y v2.y }
        Z -> { v | z = f v.z v2.z }

{-| Map with the right value taking precendence
-}
mapRAxis2 : Axis -> (v -> v2 -> v2) -> N3 v  -> N3 v2  -> N3 v2
mapRAxis2 axis f v v2 =
    case axis of 
        X -> { v2 | x = f v.x v2.x }
        Y -> { v2 | y = f v.y v2.y }
        Z -> { v2 | z = f v.z v2.z }

{-| Map with the left value taking precendence
-}
mapLAxis3 : Axis -> (v -> v2 -> v3 -> v) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v
mapLAxis3 axis f v v2 v3 =
    case axis of 
        X -> { v | x = f v.x v2.x v3.x }
        Y -> { v | y = f v.y v2.y v3.y }
        Z -> { v | z = f v.z v2.z v3.z }

{-| Map with the right value taking precendence
-}
mapRAxis3 : Axis -> (v -> v2 -> v3 -> v3) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v3
mapRAxis3 axis f v v2 v3 =
    case axis of 
        X -> { v3 | x = f v.x v2.x v3.x }
        Y -> { v3 | y = f v.y v2.y v3.y }
        Z -> { v3 | z = f v.z v2.z v3.z }

{-| Map with the left value taking precendence
-}
mapLAxis4 : Axis -> (v -> v2 -> v3 -> v4 -> v) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v4  -> N3 v
mapLAxis4 axis f v v2 v3 v4 =
    case axis of 
        X -> { v | x = f v.x v2.x v3.x v4.x }
        Y -> { v | y = f v.y v2.y v3.y v4.y }
        Z -> { v | z = f v.z v2.z v3.z v4.z }

{-| Map with the right value taking precendence
-}
mapRAxis4 : Axis -> (v -> v2 -> v3 -> v4 -> v4) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v4  -> N3 v4
mapRAxis4 axis f v v2 v3 v4 =
    case axis of 
        X -> { v4 | x = f v.x v2.x v3.x v4.x }
        Y -> { v4 | y = f v.y v2.y v3.y v4.y }
        Z -> { v4 | z = f v.z v2.z v3.z v4.z }

-- Individual fields


{-| Gets the `x` component from `d`
-}
x : N3 v -> v
x d = d.x


{-| Gets the `y` component from `d`
-}
y : N3 v -> v
y d = d.y


{-| Gets the `z` component from `d`
-}
z : N3 v -> v
z d = d.z


-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : Axis -> Lens (N3 b) b
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
        Z ->
            { name = "z"
            , get = \v -> Ok <| v.z
            , set = \v p -> Ok <| { p | z = v }
            }


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatAxisToLens : Axis -> Lens a (N3 b) -> Lens a b
concatAxisToLens axis lens =
    Lens.concat lens <| axisToLens axis



-- MAP -------------------------------------------------------------------------

{-| -arity version of map.
-}
map : (v -> out) -> N3 v  -> N3 out
map f v  =
    { x = f v.x
    , y = f v.y
    , z = f v.z
    }


{-| 2-arity version of map.
-}
map2 : (v -> v2 -> out) -> N3 v  -> N3 v2  -> N3 out
map2 f v  v2  =
    { x = f v.x v2.x
    , y = f v.y v2.y
    , z = f v.z v2.z
    }


{-| 3-arity version of map.
-}
map3 : (v -> v2 -> v3 -> out) -> N3 v  -> N3 v2  -> N3 v3  -> N3 out
map3 f v  v2  v3  =
    { x = f v.x v2.x v3.x
    , y = f v.y v2.y v3.y
    , z = f v.z v2.z v3.z
    }


{-| 4-arity version of map.
-}
map4 : (v -> v2 -> v3 -> v4 -> out) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v4  -> N3 out
map4 f v  v2  v3  v4  =
    { x = f v.x v2.x v3.x v4.x
    , y = f v.y v2.y v3.y v4.y
    , z = f v.z v2.z v3.z v4.z
    }


-- APPLY -----------------------------------------------------------------------

apply : N3 (v -> out) -> N3 v  -> N3 out
apply fns v  =
    { x = fns.x v.x
    , y = fns.y v.y
    , z = fns.z v.z
    }


apply2 : N3 (v -> v2 -> out) -> N3 v  -> N3 v2  -> N3 out
apply2 fns v  v2  =
    { x = fns.x v.x v2.x
    , y = fns.y v.y v2.y
    , z = fns.z v.z v2.z
    }


apply3 : N3 (v -> v2 -> v3 -> out) -> N3 v  -> N3 v2  -> N3 v3  -> N3 out
apply3 fns v  v2  v3  =
    { x = fns.x v.x v2.x v3.x
    , y = fns.y v.y v2.y v3.y
    , z = fns.z v.z v2.z v3.z
    }


apply4 : N3 (v -> v2 -> v3 -> v4 -> out) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v4  -> N3 out
apply4 fns v  v2  v3  v4  =
    { x = fns.x v.x v2.x v3.x v4.x
    , y = fns.y v.y v2.y v3.y v4.y
    , z = fns.z v.z v2.z v3.z v4.z
    }


-- MAP WITH AXIS ---------------------------------------------------------------

{-| Transform a the values in `v` using `fn`
-}
mapWithAxis : (Axis -> v -> out) -> N3 v  -> N3 out
mapWithAxis fn v  =
    { x = fn X v.x
    , y = fn Y v.y
    , z = fn Z v.z
    }


{-| N-arity version of mapWith
-}
mapWithAxis2 : (Axis -> v -> v2 -> out) -> N3 v  -> N3 v2  -> N3 out
mapWithAxis2 fn v  v2  =
    { x = fn X v.x v2.x
    , y = fn Y v.y v2.y
    , z = fn Z v.z v2.z
    }


{-| N-arity version of mapWith
-}
mapWithAxis3 : (Axis -> v -> v2 -> v3 -> out) -> N3 v  -> N3 v2  -> N3 v3  -> N3 out
mapWithAxis3 fn v  v2  v3  =
    { x = fn X v.x v2.x v3.x
    , y = fn Y v.y v2.y v3.y
    , z = fn Z v.z v2.z v3.z
    }


{-| N-arity version of mapWith
-}
mapWithAxis4 : (Axis -> v -> v2 -> v3 -> v4 -> out) -> N3 v  -> N3 v2  -> N3 v3  -> N3 v4  -> N3 out
mapWithAxis4 fn v  v2  v3  v4  =
    { x = fn X v.x v2.x v3.x v4.x
    , y = fn Y v.y v2.y v3.y v4.y
    , z = fn Z v.z v2.z v3.z v4.z
    }

-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> N3 v -> a
fold fn a d =
    a |> fn d.x |> fn d.y |> fn d.z 



-- LIST ------------------------------------------------------------------------


toList : N3 v -> List v
toList d =
    [ d.x, d.y, d.z ]



fromList : List v ->  Maybe (N3 v)
fromList l =
    case l of
        [ x, y, z ] -> Just <| N3 x y z
        _ -> Nothing





-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
appendUniform : (v -> v -> v) -> N3 v -> N3 v -> N3 v
appendUniform fn a b =
    { x = fn a.x b.x
    , y = fn a.y b.y
    , z = fn a.z b.z
    }





-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUsing : N3 (v -> v -> v) -> N3 v -> List (N3 v) -> N3 v
concatUsing fns empty xs =
    List.foldl (apply2 fns) empty xs


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> N3 v -> List (N3 v) -> N3 v
concatUniform fn empty xs =
    concatUsing (uniform fn) empty xs






-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> N3 v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("x",  vfn b.x)
        , ("y",  vfn b.y)
        , ("z",  vfn b.z)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (N3 v)
decode vdecoder =
    Json.Decode.map3
        N3
        (Json.Decode.field "x" vdecoder)
        (Json.Decode.field "y" vdecoder)
        (Json.Decode.field "z" vdecoder)
