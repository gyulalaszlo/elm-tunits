-- MAP -------------------------------------------------------------------------
{{#nTimes max=4 min=1 prefix="v" empty=1}}

{-| {{current}}-arity version of map.
-}
map{{current}} : ({{#join elements joiner=" -> "~}} {{value.wrapped}} {{~/join}} -> out)
    {{~#join elements}} -> {{../../name}} {{value.wrapped}} {{/join}} -> {{../name}} out
map{{current}} f
    {{~#join elements}} {{value.prefixed}} {{/join}} =
    { {{#join ../fields lines="    " joiner=", " ~}}
        {{key}} = f
            {{~#join ../elements}} {{value.prefixed}}.{{../key~}} {{/join}}
    {{~/join}}

    }

{{/nTimes}}

-- APPLY -----------------------------------------------------------------------
{{#nTimes max=4 min=1 prefix="v" empty=1}}

apply{{current}} : {{../name}} ({{#join elements joiner=" -> "~}} {{value.wrapped}} {{~/join}} -> out)
    {{~#join elements}} -> {{../../name}} {{value.wrapped}} {{/join}} -> {{../name}} out
apply{{current}} fns
    {{~#join elements}} {{value.prefixed}} {{/join}} =
    { {{#join ../fields lines="    " joiner=", " ~}}
        {{key}} = fns.{{key}}
            {{~#join ../elements}} {{value.prefixed}}.{{../key~}} {{/join}}
    {{~/join}}

    }

{{/nTimes}}

-- MAP WITH AXIS ---------------------------------------------------------------

mapWith{{>axisName}} : ({{>axisName}} -> v -> c) -> {{name}} v -> {{name}} c
mapWith{{>axisName}} f d =
    {{#>nFieldsLines first="{ " joiner=", " last="}"~}}
        {{hash.name}} = f {{upperFirst hash.name}} d.{{{hash.name}}}
    {{~/nFieldsLines}}


-- FOLD ------------------------------------------------------------------------

fold : (v -> a -> a) -> a -> {{name}} v -> a
fold fn a d =
    a {{#>nFields~}} |> fn d.{{hash.name}} {{/nFields}}



-- LIST ------------------------------------------------------------------------


toList : {{name}} v -> List v
toList d =
    [ {{#>nFields joiner=", "~}} d.{{{hash.name}}} {{~/nFields}} ]



fromList : List v ->  Maybe ({{name}} v)
fromList l =
    case l of
        [ {{#>nFields joiner=", "~}} {{{hash.name}}} {{~/nFields}} ] -> Just <| {{name}} {{~#>nFields}} {{{hash.name}}} {{~/nFields}}
        _ -> Nothing



