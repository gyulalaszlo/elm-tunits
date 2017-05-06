type {{name}}
    {{#join kinds first="    = " joiner="    | " as |name values| ~}}
        {{name}} {{join values}}
    {{~/join}}


{{#join kinds first="" joiner="" lines=true as |name values| }}

{{lowerFirst name}} : {{join values joiner=" -> "}} -> {{name}}
{{lowerFirst name}} = {{name}}

{{/join}}

{{#join kinds first="" joiner="" lines=true as |name values| }}

to{{name}} : {{name}} -> {{name}}
to{{name}} v =
    case v of
    {{#join ../kinds lines=true joiner="    " as |vName vv| }}
        {{vName}} -> {{vv}}
    {{/join}}

{{/join}}

