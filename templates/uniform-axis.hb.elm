
--------------------------------------------------------------------------------

-- Axis

type {{#if axis~}} {{axis}} {{~else~}} Axis {{~/if}}
    {{#>nFieldsLines joiner="| " first="= "~}}
        {{upperFirst hash.name}}
    {{~/nFieldsLines}}

-- Axis naming metadata

{{>axisNameLower}}Names : List String
{{>axisNameLower}}Names  = [ {{#>nFields joiner=", "~}} {{{json hash.name}}} {{~/nFields}} ]

{{>axisNameLower}}Name : {{name}}Axis -> String
{{>axisNameLower}}Name a =
    case a of
        {{#>nFieldsLines~}}
            {{upperFirst hash.name}} -> {{{json hash.name}}}
        {{~/nFieldsLines}}


-- Getters / Setters

get : {{name}}Axis -> {{name}} -> v
get a d =
    case a of
        {{#>nFieldsLines~}}
            {{upperFirst hash.name}} -> d.{{{hash.name}}}
        {{~/nFieldsLines}}

set : {{name}}Axis -> {{#>tArgs}} -> {{/tArgs}} -> {{>tName}} -> {{>tName}}
set a {{>tArgs}} d =
    case a of
        {{#>nFieldsLines~}}
            {{upperFirst hash.name}} -> { d | {{{hash.name}}} = v }
        {{~/nFieldsLines}}
