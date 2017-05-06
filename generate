let program    = require('commander');
let YAML       = require('yamljs');
// let R = require('ramda');
let Handlebars = require('handlebars');
let fs         = require('fs');
// let mapIndexed = R.addIndex( R.map );
let path       = require('path');


Handlebars.registerHelper('lowerFirst', str => str.length > 0 ? str[0].toLowerCase() + str.substr(1) : str);
Handlebars.registerHelper('upperFirst', str => str.length > 0 ? str[0].toUpperCase() + str.substr(1) : str);
Handlebars.registerHelper('json', v => JSON.stringify(v));
Handlebars.registerHelper('length', v => Array.isArray(v) ? v.length : Object.keys(v).length);


// generateData("src/SEd/Operations/OperationKindData.yaml")
function collectPairs(val, memo) {
    let [k, v] = val.split("=");
    if (v && v.length > 0 && k.length > 0) {
        memo[k] = v;
    }
    return memo;
}

program
    .version('0.0.1')
    .usage('[options] <templates>')
    .option('-D --define <key=value>', "Define local variables", collectPairs, {})
    .option('-j --json <json>', "Define local variables as json", JSON.parse, {})
    .option('-g --genfile <json>', "Use the specific genfile")
    .option('-r --replace <key>',
            "Dont overwrite the output file, but replace anything matching {{{<key>}}}. Default: yield", 'yield')
    .option("-o, --output <file>", "Set the output file", '')
    .parse(process.argv);


main(program)
    .then(v => console.log(["OK", v]))
    .catch(console.error);

function main(program) {

    let single = (p) => {
        if (!p.output) throw new Error("--output required when a single file is processed")
        return  replaceFile(p.output, Object.assign(p.json, p.define), p.args);
    }

    return (program.genfile) ? readGenFile(program.genfile) : single(program);
}

function readGenFile(genfilePath) {
    let genData = YAML.load(genfilePath);
    let genSingleOutput = (inputs, output, data) => replaceFile(output, data, inputs);
    let mapOutputs = o => Object.keys(o.outputs).map((k)=> genSingleOutput(o.inputs, k, o.outputs[k]));
    return Promise.all(genData.reduce((memo,f)=> memo.concat(mapOutputs(f)), []));
}


function replaceContent(fileName) {
    return data => new Promise((resolve, reject) => {
        if (fs.existsSync(fileName) && fs.readFileSync(fileName, "utf-8") === data) {
            resolve(["same", fileName]);
        }
        fs.writeFile(fileName, data, "utf-8", (err) => {
            return err ? reject(err) : resolve(["written", fileName]);
        });

    });
}

function readTextFile(f) {
    return new Promise((resolve, reject)=>{
        fs.readFile(f, "utf-8",(err,res)=>{
            if (err) { return reject(err); }
            resolve(res);
        });
    });
}

function template(data) {
    return text => new Promise((resolve, reject) => {
        let result = "";
        try {
            let tpl   = Handlebars.compile(text);
            result    = tpl(data);
        } catch (e) {
            return reject(e);
        }
        return resolve(result);
    });
}


function replaceFile(output, data, inFiles) {
    return Promise.all(inFiles.map(readTextFile))
                  .then(bs => bs.join("\n\n"))
                  .then(template(data))
                  .then(replaceContent(output))
}
