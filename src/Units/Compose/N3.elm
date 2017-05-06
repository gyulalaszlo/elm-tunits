module Units.Compose.N3 exposing (..)

import Json.Encode
import Json.Decode

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




--------------------------------------------------------------------------------

-- Axis

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
        

-- MAP -------------------------------------------------------------------------

map : (v -> c) -> N3 v -> N3 c
map f d =
        { x = f d.x
        , y = f d.y
        , z = f d.z
        }

mapWithAxis : (Axis -> v -> c) -> N3 v -> N3 c
mapWithAxis f d =
        { x = f X d.x
        , y = f Y d.y
        , z = f Z d.z
        }

apply : N3 (a -> b) -> N3 a -> N3 b
apply fns d =
        { x = fns.x d.x
        , y = fns.y d.y
        , z = fns.z d.z
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
