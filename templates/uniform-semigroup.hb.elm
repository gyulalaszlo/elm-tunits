-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all {{>axisName}}
-}
appendUniform : (v -> v -> v) -> {{ name }} v -> {{ name }} v -> {{ name }} v
appendUniform fn a b =
    { {{#join fields lines="    " joiner=", " as |n _| ~}}
        {{n}} = fn a.{{n}} b.{{n}}
    {{~/join}}

    }


{{#each concat}}



{-| Concatenates `a` and `b` using the supplied concatenator function
for all {{>axisName}}
-}
append{{postfix}} : {{ ../name }} {{typeParam}} -> {{ ../name }} {{typeParam}} -> {{ ../name }} {{typeParam}}
append{{postfix}}  a b =
    { {{#join using lines="    " joiner=", " as |field using| ~}}
        {{field}} = {{using}} a.{{field}} b.{{field}}
    {{~/join}}

    }

{-| Concatenates `a` and `b` using the supplied concatenator function
for all {{>axisName}}
-}
{{lowerFirst postfix}}Appender : {{ ../name }} ({{typeParam}} -> {{typeParam}} -> {{typeParam}})
{{lowerFirst postfix}}Appender =
    { {{#join using lines="    " joiner=", " as |field using| ~}}
        {{field}} = {{using}}
    {{~/join}}

    }

{{/each}}

