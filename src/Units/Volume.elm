module Units.Volume exposing (..)
{-| Describe me please...
-}

import Math.Vector3 as Vec3 exposing (Vec3)
import Units.Length exposing (Length, lengthToGL, meters)

type alias Volume
    = { x: Length, y: Length, z: Length }



--makeSize3 : v -> v -> v -> Size3D
map3d fn {x,y,z} = { x = fn x, y = fn y, z = fn z}
map3dt fn (x,y,z) = { x = fn x, y = fn y, z = fn z}
map3d3 fn x y z = { x = fn x, y = fn y, z = fn z}

size3dMeters : Float -> Float -> Float -> Volume
size3dMeters = map3d3 meters


sizeToGl : Volume -> Vec3
sizeToGl =
    map3d lengthToGL >> Vec3.fromRecord

