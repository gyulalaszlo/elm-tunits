module Units.Compose.NDimensional exposing (..)
{-| Describe me please...
-}
import Lens exposing (Lens)
import Lens.Common exposing (lensForField)
import Math.Vector3




-- Dimension: 3
--------------------------------------------------------------------------------


type alias Tri v =
    { x : v
    , y : v
    , z : v
    }



triFromUniform : v -> Tri v
triFromUniform v = {  x = v ,  y = v ,  z = v  }



{-| Creates a composite unit from component values
-}
triFrom : v -> v -> v -> Tri v
triFrom x y z = {  x = x ,  y = y ,  z = z  }


type TriAxis
    = XXX
    | YYY
    | ZZZ

triNames : List String
triNames  =
    [ "x", "y", "z" ]


triName : TriAxis -> String
triName a =
    case a of
        XXX -> "x"
        YYY -> "y"
        ZZZ -> "z"


triGet : TriAxis -> Tri v -> v
triGet a d =
    case a of
        XXX -> d.x
        YYY -> d.y
        ZZZ -> d.z


triSet : TriAxis -> v -> Tri v -> Tri v
triSet a v d =
    case a of
        XXX -> { d | x = v }
        YYY -> { d | y = v }
        ZZZ -> { d | z = v }


triMap : (v -> c) -> Tri v -> Tri c
triMap f d =
    { x = f d.x
    , y = f d.y
    , z = f d.z
    }


triAxisMap : (TriAxis -> v -> c) -> Tri v -> Tri c
triAxisMap f d =
    { x = f XXX d.x
    , y = f YYY d.y
    , z = f ZZZ d.z
    }


triApply : Tri (a -> b) -> Tri a -> Tri b
triApply fns d =
    { x = fns.x d.x
    , y = fns.y d.y
    , z = fns.z d.z
    }


triFold : (v -> a -> a) -> a -> Tri v -> a
triFold fn a d =
    a |> fn d.x |> fn d.y |> fn d.z


triToList : Tri v -> List v
triToList d =
    [ d.x, d.y, d.z]


triFromList : List v -> Maybe (Tri v) 
triFromList ls =
    case ls of
        [ x, y, z] -> Just <|Tri x y z
        _ -> Nothing



