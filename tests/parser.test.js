const lexer = require("../lexer")
const tokenizer = require("../tokenizer")
const parser = require("../parser")

const parsingDictionary = {
    whitespaces : {
        input : tokenizer(lexer("(list (+ 2 3))")),
        output : []
    },
    if : {
        input:tokenizer(lexer('(if () "Hello \\"world\\"!" "Not hello")')),
        output: [
            ]
    },
    recurse_example: {
        input: tokenizer(lexer("(first (list 1 (+ 2 3) 9))")),
        output: [
        ]
    },
    lambda: {
        input: tokenizer(lexer("((lambda (a b c) (+ a (+ b c))) 1 2 3)")),
        output: [
        ]
    }

}

// for (let parser_test of Object.keys(parsingDictionary)) {
//     test(`Testing for ${parser_test}`, () => {
//         expect(parser(parsingDictionary[parser_test]["input"])).toEqual(parsingDictionary[parser_test]["output"])
//     })
// }

// console.log(JSON.stringify(parser(parsingDictionary["whitespaces"]["input"]), null, 2))
// console.log(JSON.stringify(parser(parsingDictionary["if"]["input"]), null, 2))
// console.log(JSON.stringify(parser(parsingDictionary["recurse_example"]["input"]), null, 2))
console.log(JSON.stringify(parser(parsingDictionary["lambda"]["input"]), null, 2))