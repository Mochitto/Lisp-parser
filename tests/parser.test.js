const lexer = require("../lexer")
const tokenizer = require("../tokenizer")
const parser = require("../parser")

const parsingDictionary = {
    whitespaces : {
        input : tokenizer(lexer("(list (+ 2 3))")),
        output : []
    },
    string : {
        input:tokenizer(lexer('("Hello \\"world\\"!")')),
        output: [
            ]
    },
    recurse_example: {
        input: tokenizer(lexer("(first (list 1 (+ 2 3) 9))")),
        output: [
        ]
    }

}

for (let parser_test of Object.keys(parsingDictionary)) {
    test(`Testing for ${parser_test}`, () => {
        expect(parser(parsingDictionary[parser_test]["input"])).toEqual(parsingDictionary[parser_test]["output"])
    })
}
