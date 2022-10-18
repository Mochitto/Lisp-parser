const lexer = require("../lexer")
const tokenizer = require("../tokenizer")
const parser = require("../parser")
const [evaluate, scope] = require("../interpreter")


// (first (list "This is inside of a list" 1) 1) (defparameter *ciao* ()) *ciao* (if *ciao* "This is true!" "This is false!")
let sexpressions = parser(tokenizer(lexer('(* 1 (+ 2 5))'))).tree
let myscope = new scope()
myscope.vars["first"] = function(...array) {return array[0]}
myscope.vars["list"] = function(...array) {return array}


let number_of_sexp = 1
for (let sexpression of sexpressions) {
    console.log(`[${number_of_sexp++}]`)
    console.log(evaluate(sexpression, myscope))
}