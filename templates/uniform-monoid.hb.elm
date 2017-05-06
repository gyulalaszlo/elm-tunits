-- EMPTY AND CONCAT ------------------------------------------------------------

{{#if empty }}

empty : {{ name }} {{empty.typeParam}}
empty =
    {{#>nFieldsLines first="{ " last="}" joiner=", "~}}
        {{hash.name}} = {{ at hash.name ../empty.value }}
    {{~/nFieldsLines}}

{{/if}}

{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concat : {{ name }} (v -> v -> v) -> {{ name }} v -> List ({{ name }} v) -> {{ name }} v
concat fns empty xs =
    List.foldl (apply2 fns) empty xs

{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> {{ name }} v -> List ({{ name }} v) -> {{ name }} v
concatUniform fn empty xs =
    concat (uniform fn) empty xs
