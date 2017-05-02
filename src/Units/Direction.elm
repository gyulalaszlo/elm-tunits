module Units.Direction exposing (..)
{-| Describe me please...
-}

import Math.Vector3 as V3 exposing (Vec3)

type Direction
   = DownToUp
   | LeftToRight
   | Custom Vec3


default = DownToUp


toNormalV3 : Direction -> Vec3
toNormalV3 d =
    case d of
        DownToUp -> V3.j
        LeftToRight -> V3.i
        Custom v -> V3.normalize v




defaultVec3 = V3.j



{-
[[[cog

import cog
directions = { "DownToUp": []
             , "LeftToRight": []
             , "Custom": ["Vec3"]
             }


cog.outl("kindStrings=[ %s ]" % ", ".join(map(lambda (v,_): '"' + v + '"', directions.iteritems())))


cog.outl("""
toS a =
    case a of
""")
for name, val in directions.iteritems():
    cog.outl("        %s %s -> \"%s\"" % (name, " ".join(map(lambda a: "_", val)), name))


cog.outl("""


fromS s =
    case s of
""")
for name, val in directions.iteritems():
    cog.outl("        \"%s\" -> Ok <| %s %s" % (name, name, " ".join(map(lambda a: "default" + a, val))))

cog.outl("        _ -> Err <| \"Unknown Direction kind: \" ++ s")




cog.outl("""
kindValueLens = { name = "DirectionKind" , get = Ok << toS , set = \\a v -> if a == toS v then Ok v else fromS a }
""")


for name, val in directions.iteritems():

  isEmpty = len(val) == 0

  cog.outl("")
  cog.outl("-- %s --------------------------------------" % name)

  cog.outl("")
  cog.outl("lensFor%s =" % name)
  cog.outl("    { name = \"%s\"" % name)
  cog.outl("    , get = \\a -> case a of" )

  if isEmpty:
      cog.outl("                %s -> Ok ()" % name)
      cog.outl("                _ -> Err \"Type is not: %s\"" % name)
      cog.outl("    , set = \\_ _ -> Ok <| %s " % name)
  else:
      cog.outl("                %s vv -> Ok vv" % name)
      cog.outl("                _ -> Err \"Type is not: %s\"" % name)
      cog.outl("    , set = \\v _ -> Ok <| %s v " % name)

  cog.outl("    }")
  cog.outl("")

]]]-}
kindStrings=[ "DownToUp", "LeftToRight", "Custom" ]

toS a =
    case a of

        DownToUp  -> "DownToUp"
        LeftToRight  -> "LeftToRight"
        Custom _ -> "Custom"



fromS s =
    case s of

        "DownToUp" -> Ok <| DownToUp 
        "LeftToRight" -> Ok <| LeftToRight 
        "Custom" -> Ok <| Custom defaultVec3
        _ -> Err <| "Unknown Direction kind: " ++ s

kindValueLens = { name = "DirectionKind" , get = Ok << toS , set = \a v -> if a == toS v then Ok v else fromS a }


-- DownToUp --------------------------------------

lensForDownToUp =
    { name = "DownToUp"
    , get = \a -> case a of
                DownToUp -> Ok ()
                _ -> Err "Type is not: DownToUp"
    , set = \_ _ -> Ok <| DownToUp 
    }


-- LeftToRight --------------------------------------

lensForLeftToRight =
    { name = "LeftToRight"
    , get = \a -> case a of
                LeftToRight -> Ok ()
                _ -> Err "Type is not: LeftToRight"
    , set = \_ _ -> Ok <| LeftToRight 
    }


-- Custom --------------------------------------

lensForCustom =
    { name = "Custom"
    , get = \a -> case a of
                Custom vv -> Ok vv
                _ -> Err "Type is not: Custom"
    , set = \v _ -> Ok <| Custom v 
    }

--[[[end]]]