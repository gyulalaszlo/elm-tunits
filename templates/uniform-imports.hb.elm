module {{package}}.{{name}} exposing (..)

import Json.Encode
import Json.Decode
{{#each imports~}}
import {{.}}
{{/each}}

