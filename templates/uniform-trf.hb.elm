-- MAP -------------------------------------------------------------------------

map : (v -> c) -> {{name}} v -> {{name}} c
map f d =
    {{#>nFieldsLines first="{ " joiner=", " last="}"~}}
        {{hash.name}} = f d.{{{hash.name}}}
    {{~/nFieldsLines}}


{{>axisNameLower}}Map : ({{>axisName}} -> v -> c) -> {{name}} v -> {{name}} c
{{>axisNameLower}}Map f d =
    {{#>nFieldsLines first="{ " joiner=", " last="}"~}}
        {{hash.name}} = f {{upperFirst hash.name}} d.{{{hash.name}}}
    {{~/nFieldsLines}}


apply : {{name}} (a -> b) -> {{name}} a -> {{name}} b
apply fns d =
    {{#>nFieldsLines first="{ " joiner=", " last="}"~}}
        {{hash.name}} = fns.{{upperFirst hash.name}} d.{{{hash.name}}}
    {{~/nFieldsLines}}


-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> {{name}} v -> a
fold fn a d =
    a {{#>nFields~}} |> fn {{hash.name}} {{/nFields}}



-- LIST ------------------------------------------------------------------------


toList : {{name}} v -> List v
toList d =
    [ {{#>nFields joiner=", "~}} {{{hash.name}}} {{~/nFields}} ]

fromList : List v ->  Maybe ({{name}} v)
fromList l =
    case l of
        [ {{#>nFields joiner=", "~}} {{{hash.name}}} {{~/nFields}} ] -> Just <| {{name}} {{~#>nFields}} {{{hash.name}}} {{~/nFields}}
        _ -> Nothing