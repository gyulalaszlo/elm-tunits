module Units.Compose.N4 exposing (..)

import Json.Encode
import Json.Decode
import Math.Vector4
import Lens exposing (Lens)



type alias N4  a =
    { x : a
    , y : a
    , z : a
    , u : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> N4 a
uniform v =
    {  x = v ,  y = v ,  z = v ,  u = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  ->  a  ->  a  -> N4 a
from x y z u =
        { x = x
        , y = y
        , z = z
        , u = u
        }


zero : N4 number
zero  = uniform 0



one : N4 number
one  = uniform 1









fromVec4 : Math.Vector4.Vec4 -> N4 Float
fromVec4 input = Math.Vector4.toRecord input

toVec4 : N4 Float -> Math.Vector4.Vec4
toVec4 input = Math.Vector4.fromRecord input


{-| Lens to convert N4 Float from and to Math.Vector4.Vec4`
-}
lensForVec4 : Lens Math.Vector4.Vec4 (N4 Float)
lensForVec4 =
    { name = "Math.Vector4.Vec4 <-> N4"
    , get = \v -> Ok <| fromVec4 v
    , set = \v p -> Ok <| toVec4 v
    }






-- AXIS : Axis
--------------------------------------------------------------------------------


type Axis
        = X
        | Y
        | Z
        | U
        
-- Axis naming metadata

axisNames : List String
axisNames  = [ "x", "y", "z", "u" ]

axisName : Axis -> String
axisName a =
    case a of
        X -> "x"
        Y -> "y"
        Z -> "z"
        U -> "u"
        

-- Getters / Setters

get : Axis -> N4 a -> a
get axis d =
    case axis of
        X -> d.x
        Y -> d.y
        Z -> d.z
        U -> d.u
        
set : Axis -> a -> N4 a -> N4 a
set axis v d =
    case axis of
        X -> { d | x = v }
        Y -> { d | y = v }
        Z -> { d | z = v }
        U -> { d | u = v }
        

-- TRANSFORM A SINGLE AXIS
-- the first value always takes precendence in these maps


{-| Map with the left value taking precendence
-}
mapLAxis : Axis -> (v -> v) -> N4 v  -> N4 v
mapLAxis axis f v =
    case axis of 
        X -> { v | x = f v.x }
        Y -> { v | y = f v.y }
        Z -> { v | z = f v.z }
        U -> { v | u = f v.u }

{-| Map with the right value taking precendence
-}
mapRAxis : Axis -> (v -> v) -> N4 v  -> N4 v
mapRAxis axis f v =
    case axis of 
        X -> { v | x = f v.x }
        Y -> { v | y = f v.y }
        Z -> { v | z = f v.z }
        U -> { v | u = f v.u }

{-| Map with the left value taking precendence
-}
mapLAxis2 : Axis -> (v -> v2 -> v) -> N4 v  -> N4 v2  -> N4 v
mapLAxis2 axis f v v2 =
    case axis of 
        X -> { v | x = f v.x v2.x }
        Y -> { v | y = f v.y v2.y }
        Z -> { v | z = f v.z v2.z }
        U -> { v | u = f v.u v2.u }

{-| Map with the right value taking precendence
-}
mapRAxis2 : Axis -> (v -> v2 -> v2) -> N4 v  -> N4 v2  -> N4 v2
mapRAxis2 axis f v v2 =
    case axis of 
        X -> { v2 | x = f v.x v2.x }
        Y -> { v2 | y = f v.y v2.y }
        Z -> { v2 | z = f v.z v2.z }
        U -> { v2 | u = f v.u v2.u }

{-| Map with the left value taking precendence
-}
mapLAxis3 : Axis -> (v -> v2 -> v3 -> v) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v
mapLAxis3 axis f v v2 v3 =
    case axis of 
        X -> { v | x = f v.x v2.x v3.x }
        Y -> { v | y = f v.y v2.y v3.y }
        Z -> { v | z = f v.z v2.z v3.z }
        U -> { v | u = f v.u v2.u v3.u }

{-| Map with the right value taking precendence
-}
mapRAxis3 : Axis -> (v -> v2 -> v3 -> v3) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v3
mapRAxis3 axis f v v2 v3 =
    case axis of 
        X -> { v3 | x = f v.x v2.x v3.x }
        Y -> { v3 | y = f v.y v2.y v3.y }
        Z -> { v3 | z = f v.z v2.z v3.z }
        U -> { v3 | u = f v.u v2.u v3.u }

{-| Map with the left value taking precendence
-}
mapLAxis4 : Axis -> (v -> v2 -> v3 -> v4 -> v) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v4  -> N4 v
mapLAxis4 axis f v v2 v3 v4 =
    case axis of 
        X -> { v | x = f v.x v2.x v3.x v4.x }
        Y -> { v | y = f v.y v2.y v3.y v4.y }
        Z -> { v | z = f v.z v2.z v3.z v4.z }
        U -> { v | u = f v.u v2.u v3.u v4.u }

{-| Map with the right value taking precendence
-}
mapRAxis4 : Axis -> (v -> v2 -> v3 -> v4 -> v4) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v4  -> N4 v4
mapRAxis4 axis f v v2 v3 v4 =
    case axis of 
        X -> { v4 | x = f v.x v2.x v3.x v4.x }
        Y -> { v4 | y = f v.y v2.y v3.y v4.y }
        Z -> { v4 | z = f v.z v2.z v3.z v4.z }
        U -> { v4 | u = f v.u v2.u v3.u v4.u }

-- Individual fields


{-| Gets the `x` component from `d`
-}
x : N4 v -> v
x d = d.x


{-| Gets the `y` component from `d`
-}
y : N4 v -> v
y d = d.y


{-| Gets the `z` component from `d`
-}
z : N4 v -> v
z d = d.z


{-| Gets the `u` component from `d`
-}
u : N4 v -> v
u d = d.u


-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : Axis -> Lens (N4 b) b
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
        U ->
            { name = "u"
            , get = \v -> Ok <| v.u
            , set = \v p -> Ok <| { p | u = v }
            }


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatAxisToLens : Axis -> Lens a (N4 b) -> Lens a b
concatAxisToLens axis lens =
    Lens.concat lens <| axisToLens axis



-- MAP -------------------------------------------------------------------------

{-| -arity version of map.
-}
map : (v -> out) -> N4 v  -> N4 out
map f v  =
    { x = f v.x
    , y = f v.y
    , z = f v.z
    , u = f v.u
    }


{-| 2-arity version of map.
-}
map2 : (v -> v2 -> out) -> N4 v  -> N4 v2  -> N4 out
map2 f v  v2  =
    { x = f v.x v2.x
    , y = f v.y v2.y
    , z = f v.z v2.z
    , u = f v.u v2.u
    }


{-| 3-arity version of map.
-}
map3 : (v -> v2 -> v3 -> out) -> N4 v  -> N4 v2  -> N4 v3  -> N4 out
map3 f v  v2  v3  =
    { x = f v.x v2.x v3.x
    , y = f v.y v2.y v3.y
    , z = f v.z v2.z v3.z
    , u = f v.u v2.u v3.u
    }


{-| 4-arity version of map.
-}
map4 : (v -> v2 -> v3 -> v4 -> out) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v4  -> N4 out
map4 f v  v2  v3  v4  =
    { x = f v.x v2.x v3.x v4.x
    , y = f v.y v2.y v3.y v4.y
    , z = f v.z v2.z v3.z v4.z
    , u = f v.u v2.u v3.u v4.u
    }


-- APPLY -----------------------------------------------------------------------

apply : N4 (v -> out) -> N4 v  -> N4 out
apply fns v  =
    { x = fns.x v.x
    , y = fns.y v.y
    , z = fns.z v.z
    , u = fns.u v.u
    }


apply2 : N4 (v -> v2 -> out) -> N4 v  -> N4 v2  -> N4 out
apply2 fns v  v2  =
    { x = fns.x v.x v2.x
    , y = fns.y v.y v2.y
    , z = fns.z v.z v2.z
    , u = fns.u v.u v2.u
    }


apply3 : N4 (v -> v2 -> v3 -> out) -> N4 v  -> N4 v2  -> N4 v3  -> N4 out
apply3 fns v  v2  v3  =
    { x = fns.x v.x v2.x v3.x
    , y = fns.y v.y v2.y v3.y
    , z = fns.z v.z v2.z v3.z
    , u = fns.u v.u v2.u v3.u
    }


apply4 : N4 (v -> v2 -> v3 -> v4 -> out) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v4  -> N4 out
apply4 fns v  v2  v3  v4  =
    { x = fns.x v.x v2.x v3.x v4.x
    , y = fns.y v.y v2.y v3.y v4.y
    , z = fns.z v.z v2.z v3.z v4.z
    , u = fns.u v.u v2.u v3.u v4.u
    }


-- MAP WITH AXIS ---------------------------------------------------------------

{-| Transform a the values in `v` using `fn`
-}
mapWithAxis : (Axis -> v -> out) -> N4 v  -> N4 out
mapWithAxis fn v  =
    { x = fn X v.x
    , y = fn Y v.y
    , z = fn Z v.z
    , u = fn U v.u
    }


{-| N-arity version of mapWith
-}
mapWithAxis2 : (Axis -> v -> v2 -> out) -> N4 v  -> N4 v2  -> N4 out
mapWithAxis2 fn v  v2  =
    { x = fn X v.x v2.x
    , y = fn Y v.y v2.y
    , z = fn Z v.z v2.z
    , u = fn U v.u v2.u
    }


{-| N-arity version of mapWith
-}
mapWithAxis3 : (Axis -> v -> v2 -> v3 -> out) -> N4 v  -> N4 v2  -> N4 v3  -> N4 out
mapWithAxis3 fn v  v2  v3  =
    { x = fn X v.x v2.x v3.x
    , y = fn Y v.y v2.y v3.y
    , z = fn Z v.z v2.z v3.z
    , u = fn U v.u v2.u v3.u
    }


{-| N-arity version of mapWith
-}
mapWithAxis4 : (Axis -> v -> v2 -> v3 -> v4 -> out) -> N4 v  -> N4 v2  -> N4 v3  -> N4 v4  -> N4 out
mapWithAxis4 fn v  v2  v3  v4  =
    { x = fn X v.x v2.x v3.x v4.x
    , y = fn Y v.y v2.y v3.y v4.y
    , z = fn Z v.z v2.z v3.z v4.z
    , u = fn U v.u v2.u v3.u v4.u
    }

-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> N4 v -> a
fold fn a d =
    a |> fn d.x |> fn d.y |> fn d.z |> fn d.u 



-- LIST ------------------------------------------------------------------------


toList : N4 v -> List v
toList d =
    [ d.x, d.y, d.z, d.u ]



fromList : List v ->  Maybe (N4 v)
fromList l =
    case l of
        [ x, y, z, u ] -> Just <| N4 x y z u
        _ -> Nothing





-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
appendUniform : (v -> v -> v) -> N4 v -> N4 v -> N4 v
appendUniform fn a b =
    { x = fn a.x b.x
    , y = fn a.y b.y
    , z = fn a.z b.z
    , u = fn a.u b.u
    }





-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUsing : N4 (v -> v -> v) -> N4 v -> List (N4 v) -> N4 v
concatUsing fns empty xs =
    List.foldl (apply2 fns) empty xs


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> N4 v -> List (N4 v) -> N4 v
concatUniform fn empty xs =
    concatUsing (uniform fn) empty xs






-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> N4 v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("x",  vfn b.x)
        , ("y",  vfn b.y)
        , ("z",  vfn b.z)
        , ("u",  vfn b.u)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (N4 v)
decode vdecoder =
    Json.Decode.map4
        N4
        (Json.Decode.field "x" vdecoder)
        (Json.Decode.field "y" vdecoder)
        (Json.Decode.field "z" vdecoder)
        (Json.Decode.field "u" vdecoder)
