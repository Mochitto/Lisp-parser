const lexer = require("../lexer")
const tokenizer = require("../tokenizer")

function get_tokens(message) {
    let stream = lexer(message)
    let token_stream = tokenizer(stream)
    let tokens = []
    while (!token_stream.eof()) {
        tokens.push(token_stream.next())
    }
    return tokens
}
const testing_tokens = {
    whitespaces : {
        input : "     \n\t\n ;; this is a comment lmao \"ay\"",
        output : []
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

for (let token_test of Object.keys(testing_tokens)) {
    test(`Testing for ${token_test}`, () => {
        expect(get_tokens(testing_tokens[token_test]["input"])).toEqual(testing_tokens[token_test]["output"])
    })
}