/* 
1. Parse top-level: create a container for the AST
2. Start parsing tokens:
    - You can either have a value, a call or a binary operation on every step =>
      Check at each step if you are dealing with a function or binary exp (could be nested)
      ex. (first (list 1 2 (+ 2 (- 6 3)))) => call (call value value )
    - Check for keywords-driven expressions 

1. Parse_toplevel()
2. Parse_atom()
    - Parse_expression => check for call, check for binary op
    - Parse_keywordX() 
    - Parse_value()
    - throw error if none of the aboves


=> Get tests up to make the building easier
*/

// variables and 

function lisp_parser(tokenStream) {
  let FALSE = {type: "bool", value: false}

  return parse_toplevel()

  function parse_toplevel() {  // Starter function
    let content = []
    while (!tokenStream.eof()) {
      content.push(parse_expression())
    }
    return {type: "prog", prog: content}
  }

  function is_punc(char) {
    let tok = tokenStream.peek()
    return tok && tok.type == "punc" && (!char || tok.value == char) && tok 
    // The third requirement is used to ask for a specific punc if present
    // The last is used to return the token, if everything is True
  }

  function skip_punc(char) {
    if (is_punc(char)) tokenStream.next();
    else tokenStream.croak(`Expecting punctuation but got: "${char}"`)
  }

  function is_keyword(kw) {
    let tok = tokenStream.peek()
    return tok && tok.type == "kw" && (!kw || tok.value == kw) && tok  // The third requirement is used to ask for a specific keyword if present
  }

  function skip_kw(kw) {
    if (is_keyword(kw)) tokenStream.next();
    else tokenStream.croak(`Expecting keyword but got: "${kw}"`)
  }

  function is_op(char) {
    let tok = tokenStream.peek()
    return tok && tok.type == "op" && (!char || tok.value == char) && tok // The third requirement is used to ask for a specific punc if present
  }

  function skip_op(char) {
    if (is_op(char)) tokenStream.next()
    else tokenStream.croak(`Expecting operation but got: "${char}"`)
  }

  function unexpected(tok) {
    tokenStream.croak(`Unexpected token: "${JSON.stringify(tok)}"`)
  }

  // ------------- This function processes tokens that are inside of a container, putting them together in a list. 
  // There's the possibilty of adding a separator (in js commas and semicolons), but there's no need in lisp
  // refer to https://lisperator.net/pltut/parser/the-parser
  function delimited(start, stop, parser=parse_atom) {
    let type = null
    let content = []
    
    skip_punc(start)
    let first

    // First atom must be a keyword, function or operation
    if (parser == parse_atom) {
      if (is_keyword("if")) {first = tokenStream.next(); type = "if"}
      else if (is_keyword()) {first = tokenStream.next(); type = "function"} 
      else if (is_op()) {first = tokenStream.next(); type = "binary"}
      else if (is_punc(stop)) {skip_punc(stop); return FALSE}
      else if (is_punc(start)) {
        skip_punc(start); 
        first = is_keyword("lambda")
        if (first) {
          skip_kw("lambda")
          type= {type: "lambda", params: delimited("(", ")", parse_varname), body: parse_atom()}
          skip_punc(stop)
        }
      }
      else tokenStream.croak(`${JSON.stringify(tokenStream.next())} is not a function nor an operation`)
  }

    // Get the arguments of the keyword/function/operation
    while (!tokenStream.eof()) {
      if (is_punc(stop)) break // stop if the next token closes the contanier
      content.push(parser())
    }
    skip_punc(stop)

    // Return either a function type or a binary type
    if (type == "if") {
      if (content.length == 2) return {type: "if", cond: content[0], then: content[1]}
      else if (content.length == 3) return {type: "if", cond: content[0], then: content[1], else: content[2]}
      else if (content.length < 2) tokenStream.croak("An if statements needs at least a condition and a result")
      else tokenStream.croak("An if statement takes at most a condition and two results")
    }
    else if (type == "function") return {type: "function", name: first.value, args: content}
    else if (type == "binary") {
      if (content.lenght > 2) {tokenStream.croak("Too many arguments for binary operation")}
      return {type: "binary", operator: first.value, left: content[0], right: content[1]}
    }
    else if (content[0].type == "var") return content
    else if (typeof(type) == "object") {
      type.args = content
      return type
    }
    else unexpected(`${type} is not expected to be at the beginnnig of an S-expression`)
  }

  function parse_expression() {
    let prog = delimited("(", ")")
    return prog
  }

  function parse_atom() {
    if (is_punc("(")) return parse_expression()
    else if (is_keyword("t")||is_keyword("nil")||is_keyword("T")||is_keyword("NIL")) return parse_bool()
    else {
      let tok = tokenStream.next()
      if (tok.type == "var" || tok.type == "num" || tok.type == "str") {
        return tok
      } else unexpected(tok)
    }
  }

  function parse_bool() {
    let tok = tokenStream.next()
    return tok.value.toLowerCase() == "t" ? {type:"bool", value: true} : FALSE
  }

function parse_varname() {
  let name = tokenStream.next()
  if (name.type != "var") tokenStream.croak(`Expected variable name but got ${JSON.stringify(name)}`) 
  return name
}

function parse_lambda() {
// TODO: consider (lambda as starter and parse params, body and args together
}}

module.exports = lisp_parser

// TODO: identifiers (kw, vars, funcs) are case insensitive

// TODO: functions definitions 

/* 
atoms: bool, strings, numbers, (), identifiers, global vars
=> 
parsing_atoms {
  check if list "(" => parse list (creates nesting and recursion) ! Add corner case of empty = bool
  check if identifier => 
    if boolean kw => parse bool 
        (check value, return {type: bool, value: true or false}) NO PROB
    // THESE ARE ALL CALLED BY LISTS
    if kw => parse kw (return type: call, name: kw name, args: parse_while)
    if defun => parse defun (return kind: func, type: def, parse params[0], get body (parse while))
    if lambda => parse lambda (return type: call, name: lambda, params: parse params[0], body: parse while)
    if let => parse let (return type: call, name:let, params: parse params[0], args: params[1], body: parse while)
    if defvar => parse defvar (return type: def, kind: var, name: parse atom, ?value: parse atom)
    if defparameter => parse defparameter (return type: def, kind: var, name: parse atom, value: parse atom)
  check if identifier, number, string => return as is

  throw unexpected error
}

parsing lists (params?=no) {
  create container
  create function holder 
  create template holder?

  skip start punc
  (check for lambda/let/defun???
    if lambda: lambda goes into function holder but needs a special check!
    let is kw! just need special parsing
    definitions need special parsing but start with a kw, no need for special cheks)
  MUST start with kw or var or is lambda (need to next two times) => return a type: call and store in function holder
  
  skip end punc, if missing throw error

  return {type: call, body:container)
}

parse while(break char) {
  while not end of stream:
    if end punc, break
    else (if no params) parse_atom (will call special parsing if needed, call list if nested) and push to container
          (if params) parse_params
}

parsing params () {
  if punc, skip start punc
  if var, save var name else throw error
  if value, parse atom // This can be thrown away later if not lambda
  if punc, skip end pung
  return [{type: var, name: varname}, {type: atom type, value: value}]
}

parse top level () {
  create container

  while not end of stream
    parse atom and push to container
  
  return {type: tree, body:container}
}


lambda: ((lambda (params) body) args) => ((lambda (a b) (+ a b)) 1 2) ;3 OR ((lambda (a) a) 3) ;4
The params MUST be a list ()

local vars: (let (var var (var value)) (body: returning value)) ex. (let ((x 1)) x) ;1 OR (let (a b) a) ;NIL

global vars: Will ommit the docstring 
(defvar *name* initial-value) // can also not have an initial-value, throws error if called without value
(defparameter *name* initial-value)  // Must have value, throws error if no value

functions: (defun name-of-fun (param) "docstring" body) // docstring is omitted in this parser
ex. (defun ababa (a) a) OR (defun (a b c) (+ a (* b c)))

let, functions and lambda seem to use the same validator for their params.
If you pass (a 1) to lambda or defun, the value gets thrown away.
lambda: kw params body
function: kw params body
let: kw params body

They can be parsed as the same thing and dealt with differently on the interpreter:
let and defun do not need args, they end by themselves
lambda needs args, because it's like calling a function




*/