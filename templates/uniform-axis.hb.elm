
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


-- TRANSFORM A SINGLE AXIS
-- the first value always takes precendence in these maps

{{#nTimes max=4 min=1 prefix="v" empty=1}}

{-| Map with the left value taking precendence
-}
mapL{{>axisName ..}}{{current}} : {{>axisName ..}} -> ({{#join elements joiner=" -> "~}} {{value.wrapped}} {{~/join}} -> v)
    {{~#join elements}} -> {{../../name}} {{value.wrapped}} {{/join}} -> {{../name}} v
mapL{{>axisName ..}}{{current}} axis f
    {{~#join elements}} {{value.prefixed~}} {{/join}} =
    case axis of {{#join ../fields }}
        {{upperFirst key}} -> { v | {{key}} = f {{!}}
                {{~#join ../elements~}} {{value.prefixed}}.{{../key}} {{/join~}}
                 }
        {{~/join}}


{-| Map with the right value taking precendence
-}
mapR{{>axisName ..}}{{current}} : {{>axisName ..}} -> ({{#join elements joiner=" -> "~}} {{value.wrapped}} {{~/join}} -> v{{current}})
    {{~#join elements}} -> {{../../name}} {{value.wrapped}} {{/join}} -> {{../name}} v{{current}}
mapR{{>axisName ..}}{{current}} axis f
    {{~#join elements}} {{value.prefixed~}} {{/join}} =
    case axis of {{#join ../fields }}
        {{upperFirst key}} -> { v{{../current}} | {{key}} = f {{!}}
                {{~#join ../elements~}} {{value.prefixed}}.{{../key}} {{/join~}}
                 }
        {{~/join}}

{{/nTimes}}

-- Individual fields

{{#each fields as |t fieldName|}}

{-| Gets the `{{fieldName}}` component from `d`
-}
{{fieldName}} : {{../name}} v -> v
{{fieldName}} d = d.{{fieldName}}

{{/each}}

{{#each aggregates }}

-- AGGREGATE: {{postfix}}

{-| Gets the `{{fieldName}}` component from `d`
-}
{{postfix}} : {{../name}} {{typeParam}} -> {{type}}
{{postfix}} { {{~#join ../fields joiner=", " }} {{key}} {{/join~}} } = {{{ formula }}}

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

