-- APPEND ----------------------------------------------------------------------

{-| Concatenates `a` and `b` using the supplied concatenator function
for all {{>axisName}}
-}
appendUniform : (v -> v -> v) -> {{ name }} v -> {{ name }} v -> {{ name }} v
appendUniform fn a b =
    {{#>nFieldsLines first="{ " last="}" joiner=", "~}}
        {{hash.name}} =  fn a.{{hash.name}} b.{{hash.name}}
    {{~/nFieldsLines}}





