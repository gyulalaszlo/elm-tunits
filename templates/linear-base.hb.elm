{{#*inline "eachKind"~}}
    {{#each kinds as |v k|}}
        {{#unless @first~}} {{{../joiner}}} {{~else~}} {{{../first}}} {{~/unless~}}
        {{{@partial-block name=k types=v }}}
        {{/each}}
        {{last}}
{{~/inline~}}
