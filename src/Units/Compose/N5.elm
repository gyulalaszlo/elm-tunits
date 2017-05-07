module Units.Compose.N5 exposing (..)

import Json.Encode
import Json.Decode
import Lens exposing (Lens)



type alias N5  a =
    { x : a
    , y : a
    , z : a
    , u : a
    , v : a
    }


-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : a -> N5 a
uniform v =
    {  x = v ,  y = v ,  z = v ,  u = v ,  v = v  }

{-| Creates a composite uniform unit from component values
-}
from :  a  ->  a  ->  a  ->  a  ->  a  -> N5 a
from x y z u v =
        { x = x
        , y = y
        , z = z
        , u = u
        , v = v
        }






-- AXIS : Axis
--------------------------------------------------------------------------------


type Axis
        = X
        | Y
        | Z
        | U
        | V
        
-- Axis naming metadata

axisNames : List String
axisNames  = [ "x", "y", "z", "u", "v" ]

axisName : Axis -> String
axisName a =
    case a of
        X -> "x"
        Y -> "y"
        Z -> "z"
        U -> "u"
        V -> "v"
        

-- Getters / Setters

get : Axis -> N5 a -> a
get axis d =
    case axis of
        X -> d.x
        Y -> d.y
        Z -> d.z
        U -> d.u
        V -> d.v
        
set : Axis -> a -> N5 a -> N5 a
set axis v d =
    case axis of
        X -> { d | x = v }
        Y -> { d | y = v }
        Z -> { d | z = v }
        U -> { d | u = v }
        V -> { d | v = v }
        
-- Individual fields

{-| Gets the `x` component from `d`
-}
x : N5 v -> v
x d = d.x

{-| Gets the `y` component from `d`
-}
y : N5 v -> v
y d = d.y

{-| Gets the `z` component from `d`
-}
z : N5 v -> v
z d = d.z

{-| Gets the `u` component from `d`
-}
u : N5 v -> v
u d = d.u

{-| Gets the `v` component from `d`
-}
v : N5 v -> v
v d = d.v


-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : Axis -> Lens (N5 b) b
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
        V ->
            { name = "v"
            , get = \v -> Ok <| v.v
            , set = \v p -> Ok <| { p | v = v }
            }


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatAxisToLens : Axis -> Lens a (N5 b) -> Lens a b
concatAxisToLens axis lens =
    Lens.concat lens <| axisToLens axis



-- MAP -------------------------------------------------------------------------

{-| -arity version of map.
-}
map : (v -> out) -> N5 v  -> N5 out
map f v  =
    { x = f v.x
    , y = f v.y
    , z = f v.z
    , u = f v.u
    , v = f v.v
    }


{-| 2-arity version of map.
-}
map2 : (v -> v2 -> out) -> N5 v  -> N5 v2  -> N5 out
map2 f v  v2  =
    { x = f v.x v2.x
    , y = f v.y v2.y
    , z = f v.z v2.z
    , u = f v.u v2.u
    , v = f v.v v2.v
    }


{-| 3-arity version of map.
-}
map3 : (v -> v2 -> v3 -> out) -> N5 v  -> N5 v2  -> N5 v3  -> N5 out
map3 f v  v2  v3  =
    { x = f v.x v2.x v3.x
    , y = f v.y v2.y v3.y
    , z = f v.z v2.z v3.z
    , u = f v.u v2.u v3.u
    , v = f v.v v2.v v3.v
    }


{-| 4-arity version of map.
-}
map4 : (v -> v2 -> v3 -> v4 -> out) -> N5 v  -> N5 v2  -> N5 v3  -> N5 v4  -> N5 out
map4 f v  v2  v3  v4  =
    { x = f v.x v2.x v3.x v4.x
    , y = f v.y v2.y v3.y v4.y
    , z = f v.z v2.z v3.z v4.z
    , u = f v.u v2.u v3.u v4.u
    , v = f v.v v2.v v3.v v4.v
    }


-- APPLY -----------------------------------------------------------------------

apply : N5 (v -> out) -> N5 v  -> N5 out
apply fns v  =
    { x = fns.x v.x
    , y = fns.y v.y
    , z = fns.z v.z
    , u = fns.u v.u
    , v = fns.v v.v
    }


apply2 : N5 (v -> v2 -> out) -> N5 v  -> N5 v2  -> N5 out
apply2 fns v  v2  =
    { x = fns.x v.x v2.x
    , y = fns.y v.y v2.y
    , z = fns.z v.z v2.z
    , u = fns.u v.u v2.u
    , v = fns.v v.v v2.v
    }


apply3 : N5 (v -> v2 -> v3 -> out) -> N5 v  -> N5 v2  -> N5 v3  -> N5 out
apply3 fns v  v2  v3  =
    { x = fns.x v.x v2.x v3.x
    , y = fns.y v.y v2.y v3.y
    , z = fns.z v.z v2.z v3.z
    , u = fns.u v.u v2.u v3.u
    , v = fns.v v.v v2.v v3.v
    }


apply4 : N5 (v -> v2 -> v3 -> v4 -> out) -> N5 v  -> N5 v2  -> N5 v3  -> N5 v4  -> N5 out
apply4 fns v  v2  v3  v4  =
    { x = fns.x v.x v2.x v3.x v4.x
    , y = fns.y v.y v2.y v3.y v4.y
    , z = fns.z v.z v2.z v3.z v4.z
    , u = fns.u v.u v2.u v3.u v4.u
    , v = fns.v v.v v2.v v3.v v4.v
    }


-- MAP WITH AXIS ---------------------------------------------------------------

mapWithAxis : (Axis -> v -> c) -> N5 v -> N5 c
mapWithAxis f d =
        { x = f X d.x
        , y = f Y d.y
        , z = f Z d.z
        , u = f U d.u
        , v = f V d.v
        }

-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> N5 v -> a
fold fn a d =
    a |> fn d.x |> fn d.y |> fn d.z |> fn d.u |> fn d.v 



-- LIST ------------------------------------------------------------------------


toList : N5 v -> List v
toList d =
    [ d.x, d.y, d.z, d.u, d.v ]



fromList : List v ->  Maybe (N5 v)
fromList l =
    case l of
        [ x, y, z, u, v ] -> Just <| N5 x y z u v
        _ -> Nothing





-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all Axis
-}
appendUniform : (v -> v -> v) -> N5 v -> N5 v -> N5 v
appendUniform fn a b =
        { x =  fn a.x b.x
        , y =  fn a.y b.y
        , z =  fn a.z b.z
        , u =  fn a.u b.u
        , v =  fn a.v b.v
        }






-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concat : N5 (v -> v -> v) -> N5 v -> List (N5 v) -> N5 v
concat fns empty xs =
    List.foldl (apply2 fns) empty xs

{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> N5 v -> List (N5 v) -> N5 v
concatUniform fn empty xs =
    concat (uniform fn) empty xs



-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> N5 v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        [ ("x",  vfn b.x)
        , ("y",  vfn b.y)
        , ("z",  vfn b.z)
        , ("u",  vfn b.u)
        , ("v",  vfn b.v)
        ]


decode : Json.Decode.Decoder v -> Json.Decode.Decoder (N5 v)
decode vdecoder =
    Json.Decode.map5
        N5
        (Json.Decode.field "x" vdecoder)
        (Json.Decode.field "y" vdecoder)
        (Json.Decode.field "z" vdecoder)
        (Json.Decode.field "u" vdecoder)
        (Json.Decode.field "v" vdecoder)
