module Units.Compose.N2 exposing (..)

import Json.Encode
import Json.Decode

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




--------------------------------------------------------------------------------

-- Axis

type Dimension
        = X
        | Y
        
-- Axis naming metadata

dimensionNames : List String
dimensionNames  = [ "x", "y" ]

dimensionName : Dimension -> String
dimensionName a =
    case a of
        X -> "x"
        Y -> "y"
        

-- Getters / Setters

get : Dimension -> N2 a -> a
get axis d =
    case axis of
        X -> d.x
        Y -> d.y
        
set : Dimension -> a -> N2 a -> N2 a
set axis v d =
    case axis of
        X -> { d | x = v }
        Y -> { d | y = v }
        

-- MAP -------------------------------------------------------------------------

map : (v -> c) -> N2 v -> N2 c
map f d =
        { x = f d.x
        , y = f d.y
        }

mapWithDimension : (Dimension -> v -> c) -> N2 v -> N2 c
mapWithDimension f d =
        { x = f X d.x
        , y = f Y d.y
        }

apply : N2 (a -> b) -> N2 a -> N2 b
apply fns d =
        { x = fns.x d.x
        , y = fns.y d.y
        }

{-| Helper for applying a function for two arguments (like fold)
-}
apply2 : N2 (a -> b -> c) -> N2 a -> N2 b -> N2 c
apply2 fns a b =
        { x = fns.x a.x b.x
        , y = fns.y a.y b.y
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



{-| Concatenates `a` and `b` using the supplied concatenator function
for all Dimension
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
