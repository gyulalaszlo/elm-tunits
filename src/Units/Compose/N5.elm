module Units.Compose.N5 exposing (..)

import Json.Encode
import Json.Decode

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




--------------------------------------------------------------------------------

-- Axis

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
        

-- MAP -------------------------------------------------------------------------

map : (v -> c) -> N5 v -> N5 c
map f d =
        { x = f d.x
        , y = f d.y
        , z = f d.z
        , u = f d.u
        , v = f d.v
        }

mapWithAxis : (Axis -> v -> c) -> N5 v -> N5 c
mapWithAxis f d =
        { x = f X d.x
        , y = f Y d.y
        , z = f Z d.z
        , u = f U d.u
        , v = f V d.v
        }

apply : N5 (a -> b) -> N5 a -> N5 b
apply fns d =
        { x = fns.x d.x
        , y = fns.y d.y
        , z = fns.z d.z
        , u = fns.u d.u
        , v = fns.v d.v
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
