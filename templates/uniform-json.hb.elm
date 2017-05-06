
-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> {{name}} v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        {{#>nFieldsLines first="[ " joiner=", " last="]"~}}
            ({{{json hash.name}}},  vfn b.{{hash.name}})
        {{~/nFieldsLines}}



decode : Json.Decode.Decoder v -> Json.Decode.Decoder ({{name}} v)
decode vdecoder =
    Json.Decode.map{{length fields}}
        {{name}}
        {{#each fields as |v k|}}
        (Json.Decode.field "{{k}}" vdecoder)
        {{/each}}
