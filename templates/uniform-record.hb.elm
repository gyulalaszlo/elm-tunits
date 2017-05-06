type alias {{name}} {{#each typeParams}} {{.~}} {{/each}} =
    {{#each fields as |v k|}}
    {{#if @first}}{ {{else}}, {{/if~}}
    {{k}} : {{v}}
    {{/each}}
    }
