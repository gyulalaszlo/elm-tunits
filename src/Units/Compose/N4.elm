module Units.Compose.N4 exposing (..)

import Json.Encode
import Json.Decode

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




--------------------------------------------------------------------------------

-- Axis

type Dimension
        = X
        | Y
        | Z
        | U
        
-- Axis naming metadata

dimensionNames : List String
dimensionNames  = [ "x", "y", "z", "u" ]

dimensionName : Dimension -> String
dimensionName a =
    case a of
        X -> "x"
        Y -> "y"
        Z -> "z"
        U -> "u"
        

-- Getters / Setters

get : Dimension -> N4 a -> a
get axis d =
    case axis of
        X -> d.x
        Y -> d.y
        Z -> d.z
        U -> d.u
        
set : Dimension -> a -> N4 a -> N4 a
set axis v d =
    case axis of
        X -> { d | x = v }
        Y -> { d | y = v }
        Z -> { d | z = v }
        U -> { d | u = v }
        

-- MAP -------------------------------------------------------------------------

map : (v -> c) -> N4 v -> N4 c
map f d =
        { x = f d.x
        , y = f d.y
        , z = f d.z
        , u = f d.u
        }

mapWithDimension : (Dimension -> v -> c) -> N4 v -> N4 c
mapWithDimension f d =
        { x = f X d.x
        , y = f Y d.y
        , z = f Z d.z
        , u = f U d.u
        }

apply : N4 (a -> b) -> N4 a -> N4 b
apply fns d =
        { x = fns.x d.x
        , y = fns.y d.y
        , z = fns.z d.z
        , u = fns.u d.u
        }

{-| Helper for applying a function for two arguments (like fold)
-}
apply2 : N4 (a -> b -> c) -> N4 a -> N4 b -> N4 c
apply2 fns a b =
        { x = fns.x a.x b.x
        , y = fns.y a.y b.y
        , z = fns.z a.z b.z
        , u = fns.u a.u b.u
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



{-| Concatenates `a` and `b` using the supplied concatenator function
for all Dimension
-}
appendUniform : (v -> v -> v) -> N4 v -> N4 v -> N4 v
appendUniform fn a b =
        { x =  fn a.x b.x
        , y =  fn a.y b.y
        , z =  fn a.z b.z
        , u =  fn a.u b.u
        }

-- EMPTY AND CONCAT ------------------------------------------------------------


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concat : N4 (v -> v -> v) -> N4 v -> List (N4 v) -> N4 v
concat fns empty xs =
    List.foldl (apply2 fns) empty xs

{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> N4 v -> List (N4 v) -> N4 v
concatUniform fn empty xs =
    concat (uniform fn) empty xs
