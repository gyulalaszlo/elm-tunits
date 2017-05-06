-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : {{#>tArgs}} -> {{/tArgs}} -> {{name}} {{>tArgs}}
uniform v =
    { {{#>nFields joiner=", "}} {{hash.name}} = v {{/nFields}} }

{-| Creates a composite uniform unit from component values
-}
from : {{#>nFields joiner=" -> "}} {{hash.type}} {{/nFields}} -> {{name}} {{>tArgs}}
from v = {  x = v ,  y = v ,  z = v  }


