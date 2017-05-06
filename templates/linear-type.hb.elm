
-- TYPE ------------------------------------------------------------------------


{-| Linear unit for
-}
type {{name}}
    {{#join kinds lines="    " first="    = " joiner="| " as |name values| ~}}
        {{name}} {{join values.args joiner=" "}}
    {{~/join}}

-- CONSTRUCTORS

{{#join kinds first="" joiner="" lines="" as |kindName values| }}

{-| Create a new value
-}
{{lowerFirst kindName}} : {{#with values.args~}}
        {{join values.args joiner=" -> " }} -> {{ ../../name }}
        {{else~}}
        {{../name }}
    {{/with}}
{{lowerFirst kindName}} = {{kindName}}

{{/join}}

-- CONVERSIONS

{-| Converts `v` to its normalized value
-}
normalize : {{name}} -> {{normalizesTo}}
normalize v =
    case v of
    {{#join kinds lines="" as |vName vv|}}
        {{vName}} {{#join vv.args}} v{{key~}} {{/join}} -> {{vv.normalize}}
    {{~/join}}


{{#join kinds first="" joiner="" lines="" as |kindName values| }}


{-| Converts `v` to be in {{name}}
-}
to{{kindName}} : {{../name}} -> {{../name}}
to{{kindName}} v =
    let input = normalize v
    in case v of
    {{#join ../kinds lines="" as |vName vv|}}
        {{vName}} {{#join vv.args~}} v{{key}} {{/join~}}
        -> {{kindName}} <| let input = {{vv.normalize}} in {{value.denormalize}}
    {{~/join}}



{-| Converts `v` to be in {{name}}
-}
in{{kindName}} : {{../name}} -> {{../normalizesTo}}
in{{kindName}} v =
    case v of
    {{#join ../kinds lines="" as |vName vv|}}
        {{vName}} {{#join vv.args~}} v{{key}} {{/join~}} -> {{vv.normalize}}
    {{~/join}}


{{/join}}

