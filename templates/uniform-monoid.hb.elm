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
concatUsing : {{ name }} (v -> v -> v) -> {{ name }} v -> List ({{ name }} v) -> {{ name }} v
concatUsing fns empty xs =
    List.foldl (apply2 fns) empty xs


{-| Concatenates `a` and `b` using the supplied concatenator function pack.
-}
concatUniform : (v -> v -> v) -> {{ name }} v -> List ({{ name }} v) -> {{ name }} v
concatUniform fn empty xs =
    concatUsing (uniform fn) empty xs


{{#each concat}}

-- CONCAT SPECIALIZATION FOR: {{name}} {{ typeParam }}

{{#if empty ~}}

{-| Empty value for {{postfix}} specializations
-}
empty{{postfix}} : {{ ../name }} {{typeParam}}
empty{{postfix}} =
    { {{#join empty indent=1 joiner=", " as |f init| ~}}
        {{f}} = {{init}}
    {{~/join}}

    }

{{~/if}}


{-| Concatenates `a` and `b` using the supplied concatenator function
for all {{>axisName}}
-}
concat{{postfix}} : List ({{ ../name }} {{typeParam}}) -> {{ ../name }} {{typeParam}}
concat{{postfix}} xs =
    List.foldl (apply2 {{lowerFirst postfix}}Appender) empty{{postfix}} xs


{{/each}}

