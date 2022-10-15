const lexer = require("../lexer")
const tokenizer = require("../tokenizer")
const parser = require("../parser")

const parsingDictionary = {
    Variables : {
        input : tokenizer(lexer("(dEfPaRameter *global* (* 2 12)) (dEfVar *global*)")),
        output : {
            "type": "root",
            "tree": [
              {
                "type": "def",
                "kind": "globalVar",
                "name": "*global*",
                "value": {
                  "type": "binary",
                  "operator": "*",
                  "left": {
                    "type": "num",
                    "value": 2
                  },
                  "right": {
                    "type": "num",
                    "value": 12
                  }
                }
              },
              {
                "type": "def",
                "kind": "globalVar",
                "name": "*global*",
                "value": false
              }
            ]
          }
    },
    if_false : {
        input:tokenizer(lexer('(If () "Hello \\"world\\"!" "Not hello")')),
        output: {
            type: "root",
            tree: [
                {
                    type: "if",
                    cond: {
                        type: "bool",
                        value: false
                    },
                    then: {
                        type: "str",
                        value: 'Hello \"world\"!'
                    },
                    else: {
                        type: "str",
                        value: 'Not hello'
                    }
                }
            ]
        }
    },
    if_true : {
        input:tokenizer(lexer('(If T "Hello \\"world\\"!" "Not hello")')),
        output: {
            type: "root",
            tree: [
                {
                    type: "if",
                    cond: {
                        type: "bool",
                        value: true
                    },
                    then: {
                        type: "str",
                        value: 'Hello \"world\"!'
                    },
                    else: {
                        type: "str",
                        value: 'Not hello'
                    }
                }
            ]
        }
    },
    return_false: {
        input: tokenizer(lexer("(defun return-false ((a 1) (b 2)) NIL)")),
        output: {
            "type": "root",
            "tree": [
                {
                    "type": "def",
                    "kind": "func", 
                    "name": "return-false", 
                    "params": [
                        {
                            "type": "var", "value": "a"
                        }, 
                        {
                            "type": "var", "value": "b"
                        }
                    ],
                    "body": [
                        {
                            "type": "bool", "value": false
                        }
                    ]
                }
            ]
        }
    },
    recurse_example: {
        input: tokenizer(lexer("(first (list 1 (+ 2 3) 9))")),
        output: {
            "type": "root",
            "tree": [
              [
                {
                  "type": "call",
                  "name": "first"
                },
                [
                  {
                    "type": "call",
                    "name": "list"
                  },
                  {
                    "type": "num",
                    "value": 1
                  },
                  {
                    "type": "binary",
                    "operator": "+",
                    "left": {
                      "type": "num",
                      "value": 2
                    },
                    "right": {
                      "type": "num",
                      "value": 3
                    }
                  },
                  {
                    "type": "num",
                    "value": 9
                  }
                ]
              ]
            ]
          }
    },
    lambda: {
        input: tokenizer(lexer("((lambda ((a 1) b c) (+ a (+ b c))) 1 2 3)")),
        output: {
            "type": "root",
            "tree": [
              [
                {
                  "type": "call",
                  "name": "lambda",
                  "params": [
                    {
                      "type": "var",
                      "value": "a"
                    },
                    {
                      "type": "var",
                      "value": "b"
                    },
                    {
                      "type": "var",
                      "value": "c"
                    }
                  ],
                  "body": [
                    {
                      "type": "binary",
                      "operator": "+",
                      "left": {
                        "type": "var",
                        "value": "a"
                      },
                      "right": {
                        "type": "binary",
                        "operator": "+",
                        "left": {
                          "type": "var",
                          "value": "b"
                        },
                        "right": {
                          "type": "var",
                          "value": "c"
                        }
                      }
                    }
                  ]
                },
                {
                  "type": "num",
                  "value": 1
                },
                {
                  "type": "num",
                  "value": 2
                },
                {
                  "type": "num",
                  "value": 3
                }
              ]
            ]
          }
          
    }

}

const parsingErrors = {
    Variables_error : {
        input : tokenizer(lexer("(dEfPaRameter *global*) (dEfVar *global*))")),
        output : "Defparameter requires a value. Only variable was given."
    }
}

for (let parser_test of Object.keys(parsingDictionary)) {
    test(`Testing for ${parser_test}`, () => {
        expect(parser(parsingDictionary[parser_test]["input"])).toEqual(parsingDictionary[parser_test]["output"])
    })
}

for (let parser_test of Object.keys(parsingErrors)) {
    test(`Testing for ${parser_test}`, () => {
        expect(() => {parser(parsingErrors[parser_test]["input"])}).toThrow(parsingErrors[parser_test]["output"])
    })
}