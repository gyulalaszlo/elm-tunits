module Units.Compose.Tri exposing (..)
{-| Describe me please...
-}

import Lens exposing (Lens)
import Lens.Common exposing (lensForField)
import Math.Vector3

type alias Tri v = { x: v, y: v, z: v}

make : v -> Tri v
make v = { x = v, y = v, z = v}

make3 : v -> v -> v -> Tri v
make3 v vv vvv = { x = v, y = vv, z = vvv}

type TriAxis = XX | YY | ZZ

axisNames : List String
axisNames = ["x", "y", "z"]

axisName : TriAxis -> String
axisName t =
    case t of
        XX -> "x"
        YY -> "y"
        ZZ -> "z"

get : TriAxis -> Tri v -> v
get a t =
    case a of
        XX -> t.x
        YY -> t.y
        ZZ -> t.z

set : TriAxis -> v -> Tri v -> Tri v
set a v t =
    case a of
        XX -> { t | x = v }
        YY -> { t | y = v }
        ZZ -> { t | z = v }


{-| Transforms the values of a Tri using `fn`
-}
map : (v -> c) -> Tri v -> Tri c
map fn t =
    { x = fn t.x
    , y = fn t.y
    , z = fn t.z
    }

{-| Transforms the values of a Tri using `fn`
-}
axisMap : (TriAxis -> v -> c) -> Tri v -> Tri c
axisMap fn t =
    { x = fn XX t.x
    , y = fn YY t.y
    , z = fn ZZ t.z
    }
{-| Applies a the functions in a function Tri to a value Tri's corresponding elements
-}
apply : Tri (v -> c) -> Tri v -> Tri c
apply fns t =
    { x = fns.x t.x
    , y = fns.y t.y
    , z = fns.z t.z
    }


fold : (v -> a -> a) -> a -> Tri v -> a
fold fn a tri =
    a |> fn tri.x |> fn tri.y |> fn tri.z



toList : Tri v -> List v
toList t = [t.x,t.y, t.z]


axisToLens : TriAxis -> Lens (Tri b) b
axisToLens a =
    case a of
        XX -> lensForField.x
        YY -> lensForField.y
        ZZ -> lensForField.z

{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concatAxisToLens : TriAxis -> Lens a (Tri b) -> Lens a b
concatAxisToLens axis lens =
    Lens.concat lens <| axisToLens axis


fromVec3 : Math.Vector3.Vec3 -> Tri Float
fromVec3 = Math.Vector3.toRecord

toVec3 : Tri Float -> Math.Vector3.Vec3
toVec3 = Math.Vector3.fromRecord

vec3Lens : Lens Math.Vector3.Vec3 (Tri Float)
vec3Lens = { name = "V3 <-> Tri", get = \v -> Ok <| fromVec3 v, set = \v p -> Ok <| toVec3 v }

--type alias LensTraits v componentTraits x =
--    { x
--    | componentLens: Lens a v
--    , componentTraits: componentTraits
--    }


