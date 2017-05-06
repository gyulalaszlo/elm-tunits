
-- JSON ENCODE / DECODE

encode : (v -> Json.Encode.Value) -> {{name}} v -> Json.Encode.Value
encode vfn b =
    Json.Encode.object
        {{#each fields}}
        {{#if @first}}[ {{else}}, {{/if~}}
        ("{{.}}", vfn b.{{.}})
        {{/each}}
        ]

decode : Json.Decode.Decoder v -> Json.Decode.Decoder ({{name}} v)
decode vdecoder =
    Json.Decode.map2
        {{name}}
        {{#each fields as |v k|}}
        (Json.Decode.field "{{k}}" vdecoder)
        {{/each}}
