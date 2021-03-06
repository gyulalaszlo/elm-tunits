
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
to{{kindName}} v = {{#if values.args }}
    let input = normalize v
    in case v of
    {{#join ../kinds lines="" as |vName vv|}}
        {{vName}} {{#join vv.args~}} v{{key}} {{/join~}} ->
            let input = {{vv.normalize}}
            in {{kindName}} {{#join values.denormalize}} ({{value}}){{/join}}
    {{/join}}
{{else~}}
    {{kindName}}
{{/if}}

{{#if values.args}}

{-| Converts `v` to be in {{name}}
-}
in{{kindName}} : {{../name}} -> ({{join values.args joiner=", "}})
in{{kindName}} v =
    case v of
    {{#join ../kinds lines="" as |vName vv|}}
        {{vName}} {{#join vv.args~}} v{{key}} {{/join~}} ->
            let input = {{vv.normalize}}
            in ({{join values.denormalize joiner=", "}})
    {{/join}}


{{/if}}

{{~/join}}

