
--------------------------------------------------------------------------------

-- Axis

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
