-- CONSTRUCTORS

{-| Creates a composite uniform unit from a single value for all components
-}
uniform : {{#>tArgs}} -> {{/tArgs}} -> {{name}} {{>tArgs}}
uniform v =
    { {{#>nFields joiner=", "}} {{hash.name}} = v {{/nFields}} }

{-| Creates a composite uniform unit from component values
-}
from : {{#>nFields joiner=" -> "}} {{hash.type}} {{/nFields}} -> {{name}} {{>tArgs}}
from {{#>nFields~}} {{hash.name}} {{/nFields~}} =
    {{#>nFieldsLines first="{ " last="}" joiner=", "~}}
        {{hash.name}} = {{ hash.name }}
    {{~/nFieldsLines}}


{{#each alternatives~}}


{{#*inline "dimType"~}}
{{../name}} {{#with dimensionType~}} {{.}} {{~else~}} Float {{/with}}
{{~/inline}}

from{{name}} : {{type}} -> {{>dimType}}
from{{name}} input = {{from}}

to{{name}} : {{>dimType}} -> {{type}}
to{{name}} input = {{ to }}


{-| Lens to convert {{>dimType}} from and to {{type}}`
-}
lensFor{{name}} : Lens {{type}} ({{>dimType}})
lensFor{{name}} =
    { name = "{{ type }} <-> {{ ../name }}"
    , get = \v -> Ok <| from{{name}} v
    , set = \v p -> Ok <| to{{name}} v
    }

{{/each}}


