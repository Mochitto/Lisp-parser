const lexer = require("../lexer")
const tokenizer = require("../tokenizer")
const parser = require("../parser")


// TODO: Get tests for each case

const parsingDictionary = {
    whitespaces : {
        input : "(1 2 3)",
        output : [{
            type: "prog",
            prog: []
        }]
    },
    string : {
        input:'("Hello \\"world\\"!")',
        output: [
            {type: "punc", value: "("}, 
            {type: "str", value: "Hello \"world\"!"}, 
            {type: "punc", value: ")"}
            ]
    },
    recurse_example: {
        input: "(first (list 1 (+ 2 3) 9))",
        output: [
            {type: "punc", value: "("},
            {type: "kw", value: "first"},
            {type: "punc", value: "("},
            {type: "kw", value: "list"},
            {type: "num", value: 1},
            {type: "punc", value: "("},
            {type: "op", value: "+"},
            {type: "num", value: 2},
            {type: "num", value: 3},
            {type: "punc", value: ")"},
            {type: "num", value: 9},
            {type: "punc", value: ")"},
            {type: "punc", value: ")"}
        ]
    }

}

for (let parser_test of Object.keys(parsingDictionary)) {
    test(`Testing for ${parser_test}`, () => {
        expect(get_tokens(parsingDictionary[parser_test]["input"])).toEqual(parsingDictionary[parser_test]["output"])
    })
}