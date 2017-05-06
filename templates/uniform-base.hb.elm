{{#*inline "tArgs"~}}
    {{#each typeParams~}} {{#unless @first~}} {{{@partial-block}}} {{~/unless~}} {{.~}} {{~/each}}
{{~/inline~}}

{{#*inline "nFields"~}}
    {{#each fields as |v k|~}} {{#unless @first~}} {{{../joiner}}} {{~/unless~}} {{{@partial-block name=k type=v }}} {{~/each}}
{{~/inline~}}

{{#*inline "nFieldsLines"~}}
    {{#each fields as |v k|}}
        {{#unless @first~}} {{{../joiner}}} {{~else~}} {{{../first}}} {{~/unless~}}
        {{{@partial-block name=k type=v}}}
        {{/each}}
        {{last}}
{{~/inline~}}

{{#*inline "tName"~}}
    {{name}} {{>tArgs}}
{{~/inline~}}



{{#*inline "axisName"}}{{#if axis}}{{axis}}{{else}}Axis{{/if}}{{/inline~}}
{{#*inline "axisNameLower"}}{{#if axis~}} {{lowerFirst axis}} {{~else~}} axis {{~/if}}{{/inline~}}
