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
{{#nTimes max=4 min=1 prefix="v" empty=1}}

{{#unless current}}
{-| Transform a the values in `v` using `fn`
-}
{{else}}
{-| N-arity version of mapWith{{axisName}}
-}
{{/unless}}
mapWith{{>axisName}}{{current}} : ({{>axisName ..}} -> {{#join elements joiner=" -> "~}} {{value.wrapped}} {{~/join}} -> out)
    {{~#join elements}} -> {{../../name}} {{value.wrapped}} {{/join}} -> {{../name}} out
mapWith{{>axisName}}{{current}} fn
    {{~#join elements}} {{value.prefixed}} {{/join}} =
    { {{#join ../fields lines="    " joiner=", " ~}}
        {{key}} = fn {{upperFirst key}}
            {{~#join ../elements}} {{value.prefixed}}.{{../key~}} {{/join}}
    {{~/join}}

    }

{{/nTimes}}
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



