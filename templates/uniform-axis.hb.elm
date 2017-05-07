
-- AXIS : {{>axisName}}
--------------------------------------------------------------------------------


type {{>axisName}}
    {{#>nFieldsLines joiner="| " first="= "~}}
        {{upperFirst hash.name}}
    {{~/nFieldsLines}}

-- Axis naming metadata

{{>axisNameLower}}Names : List String
{{>axisNameLower}}Names  = [ {{#>nFields joiner=", "~}} {{{json hash.name}}} {{~/nFields}} ]

{{>axisNameLower}}Name : {{>axisName}} -> String
{{>axisNameLower}}Name a =
    case a of
        {{#>nFieldsLines~}}
            {{upperFirst hash.name}} -> {{{json hash.name}}}
        {{~/nFieldsLines}}


-- Getters / Setters

get : {{>axisName}} -> {{name}} {{>tArgs}} -> {{>tArgs}}
get axis d =
    case axis of
        {{#>nFieldsLines~}}
            {{upperFirst hash.name}} -> d.{{{hash.name}}}
        {{~/nFieldsLines}}

set : {{>axisName}} -> {{#>tArgs}} -> {{/tArgs}} -> {{>tName}} -> {{>tName}}
set axis v d =
    case axis of
        {{#>nFieldsLines~}}
            {{upperFirst hash.name}} -> { d | {{{hash.name}}} = v }
        {{~/nFieldsLines}}

-- Individual fields

{{#each fields as |t fieldName|}}
{-| Gets the `{{fieldName}}` component from `d`
-}
{{fieldName}} : {{../name}} v -> v
{{fieldName}} d = d.{{fieldName}}

{{/each}}

-- Lens for each axis

{-| Returns a new lens for the axis
-}
axisToLens : {{>axisName}} -> Lens ({{name}} b) b
axisToLens a =
    case a of
    {{#join fields as |name type| }}
        {{upperFirst name}} ->
            { name = {{ json name }}
            , get = \v -> Ok <| v.{{name}}
            , set = \v p -> Ok <| { p | {{name}} = v }
            }
    {{/join}}


{-| returns a new lens for the given axis inside the Tri pointed to by `lens`
-}
concat{{>axisName}}ToLens : {{>axisName}} -> Lens a ({{name}} b) -> Lens a b
concat{{>axisName}}ToLens axis lens =
    Lens.concat lens <| axisToLens axis

