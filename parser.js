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
  return parse_toplevel()

  function parse_toplevel() {  // Starter function
    let content = []
    while (!tokenStream.eof()) {
      content.push(parse_atom())
    }
    return {type: "root", tree: content}
  }

  function is_punc(char) {
    let tok = tokenStream.peek()
    return tok.type == "punc" && (!char || tok.value == char) && tok 
  }

  function skip_punc(char, place="") {
    
    
    if (is_punc(char)) tokenStream.next();
    else tokenStream.croak(`Expecting punctuation but got: "${JSON.stringify(tokenStream.peek())}"`)
  }

  function is_keyword(kw) {
    let tok = tokenStream.peek()
    return tok.type == "kw" && (!kw || tok.value == kw) && tok 
  }

  function is_op(char) {
    let tok = tokenStream.peek()
    return tok.type == "op" && (!char || tok.value == char) && tok 
  }

  function is_atom() {
    let tok = tokenStream.peek()
    return tok.type == "var" || tok.type == "str" || tok.type == "num"
  }

  function get_global_varname() {
    let tok = tokenStream.next()
    if (tok.type != "globalVar") tokenStream.croak(`Expected global var but got ${JSON.stringify(tok)}`)
    return tok.value
  }

  function get_varname() {
    let tok = tokenStream.next()
    if (tok.type != "var") tokenStream.croak(`Expected var but got ${JSON.stringify(tok)}`)
    return tok.value
  }
    
  function skip_op(char) {
    if (is_op(char)) tokenStream.next()
    else tokenStream.croak(`Expecting operation but got: "${JSON.stringify(tokenStream.peek())}"`)
  }

  function skip_kw(char) {
    if (is_keyword(char)) tokenStream.next()
    else tokenStream.croak(`Expecting operation but got: "${JSON.stringify(tokenStream.peek())}"`)
  }

  function unexpected(tok) {
    tokenStream.croak(`Unexpected token: "${JSON.stringify(tok)}"`)
  }

  function parse_atom() {
    

    if (is_punc("(")) return parse_sExpression()
    else if (is_atom()) {
      return tokenStream.next()
    }
    else if (is_op()) {
      let op = tokenStream.next().value
      let atoms = parse_until(")")
      if (atoms.length != 2) tokenStream.croak(`Binary operations need two arguments. ${atoms.length} given.`)
      let left = atoms[0]
      let right = atoms[1]

      if ((left.type == "num" || left.type == "var" || left.length) || (right.type == "num" || left.type == "var" || right.length)) return {type: "binary", operator: op, left: left, right: right} // Allows only numbers or non-empty arrays
      else tokenStream.croak(`Binary operations are possible only between numbers.\nLeft side: ${JSON.stringify(left)}\nRight side: ${JSON.stringify(right)}`)
    }
    else if (is_keyword("if")) {
      skip_kw("if")
      let atoms = parse_until(")")
      if (atoms.length == 2) return {type: "if", cond: atoms[0], then: atoms[1]}
      else if (atoms.length == 3) return {type: "if", cond: atoms[0], then: atoms[1], else: atoms[2]}
      else if (atoms.length < 2) tokenStream.croak(`An if statements needs at least a condition and a result. ${JSON.stringify(atoms)}`)
      else tokenStream.croak("An if statement takes at most a condition and two results")
    }
    else if (is_keyword()) {
      let keyword = tokenStream.next().value
      switch (keyword) {
        // Booleans
        case ("nil"): 
        case ("t"): 
          return {type: "bool", value: keyword == "t" ? true : false}
        // Def global vars
        case ("defvar"): 
          let ret = {type: "def", kind: "globalVar", name: get_varname()}
          if (tokenStream.peek().value != ")") ret.value = parse_atom()
          else ret.value = false
          return ret 
        case ("defparameter"): 
        let variable = get_varname()
        if (tokenStream.peek().value == ")") throw Error("Defparameter requires a value. Only variable was given.")
        else {
          let value  = parse_atom()
          return {type: "def", kind: "globalVar", name: variable, value: value}
        }
        // Def functions
        case ("defun"): {
          let name = get_varname()
          let [params, args] = parse_params()
          return {type: "def", kind: "func", name: name, params: params, body: parse_until(")")}
        }
        // Lambda and Let
        case ("lambda"): {
          let [params, args] = parse_params()
          return {type:"lambda", name:"lambda", params: params, body: parse_until(")")}
        }
        case ("let"): { 
          let [params, args] = parse_params()
          return [{type: "call", name:"lambda", params: params, body: parse_until(")")}, ...args]
        }
        // Other function calls
        default:
          return {type: "call", name: keyword}
      }
    } else unexpected(tokenStream.next())
  }

  function parse_sExpression() {
    let container = []

    skip_punc("(") // add promise to check for no call
    if (is_punc(")")) {tokenStream.next(); return {type: "bool", value: false}}
    
    // check if first type is in a given array? => only functions would return objects, if list it means (()) (should fix let tho) 
    let first = parse_atom()

    if (first.length) {skip_punc(")"); return first} // no need to get container
    else if (first.type == "lambda") {skip_punc(")"); first.type = "call"; return first} // no need to get container
    else if (first.type == "call") {} // ok, only type that needs to continue
    else if (first.type == "def") {skip_punc(")"); return first} // no need for container
    else if (first.type == "if") {skip_punc(")"); return first} // no need for container
    else if (first.type == "binary") {skip_punc(")"); return first} // no need for container
    else throw new Error(`${first} can't be at the beginning of a S-expression`) // Avoids bools corner case

    container.push(first, ...parse_until(")"))
    skip_punc(")")

    return container
  }

  function parse_until(char, parser=parse_atom) {
    let container = []
    while (!tokenStream.eof()) {
      if (tokenStream.peek().value == char) {break}
      else {
        let atom = parser()
        container.push(atom)} 
    }
    return container
  }

  function parse_params() {
    skip_punc("(")
    let params = []
    let args = []
    while (!is_punc(")")) {
      if (is_punc("(")) { // Used by let ((a 1) (b 2) c)
        skip_punc("(") 
        let content = parse_until(")")
        if (content.length > 2 || content.length == 0) tokenStream.croak(`Functions' parameters must have at most a name and a value at least a value. ${content.length} atoms found.`)
        if (content[0].type != "var") tokenStream.croak(`Functions' parameters must be in this order: var atom. ${content[0]} was found as first.`)
        if (content[1].type == "var") tokenStream.croak(`Functions' parameters must be in this order: var atom. The variable ${content[1]} was found as second.`)
        params.push(content[0])
        args.push(content[1] ? content[1] : false)
        skip_punc(")")
      } else { // (a b c)
        let token = tokenStream.next()
        if (token.type != "var") tokenStream.croak(`Functions' parameters must be variables. ${JSON.stringify(tok)} was found.`)
        params.push(token)
        args.push(false)
      }
    }
    skip_punc(")")
    return [params, args]
    }
  }

module.exports = lisp_parser

/* 
atoms: bool, strings, numbers, (), identifiers, global vars
=> 
parsing_atoms {
  check if list "(" => parse list (creates nesting and recursion) ! Add corner case of empty = bool
  check if kw => 
    Y if boolean kw => parse bool 
        (check value, return {type: bool, value: true or false}) NO PROB
    // THESE ARE ALL CALLED INSIDE OF LIST
    Y if kw => parse kw (return type: call, name: kw name, args: parse_while)
    Y if defun => parse defun (return kind: func, type: def, parse params[0], get body (parse while))
    Y if lambda => parse lambda (return type: call, name: lambda, params: parse params[0], body: parse while)
    Y if let => parse let (return type: call, name:let, params: parse params[0], args: params[1], body: parse while)
    Y if defvar => parse defvar (return type: def, kind: var, name: parse atom, ?value: parse atom)
    Y if defparameter => parse defparameter (return type: def, kind: var, name: parse atom, value: parse atom)
  check if identifier, number, string => return as is

  throw unexpected error
}

parsing lists (params?=no) {
  create container

  skip start punc
  (check for lambda/let/defun???
    if var => turn var token into call token, make it throw error based on scope in interpreter
    if lambda: check return value from parse atom and if type: call and name:lambda, skip end punc and return this (works both for lambda and let)
    let is lambda just need special parsing
    definitions need special parsing but start with a kw, no need for special checks)
  MUST start with kw or var or is lambda (need to next two times) => return a type: call and store in function holder
  
  skip end punc, if missing throw error

  return {type: call, body:container! if it was let, container is already included)
}

parse until(break char) {
  create container
  while not end of stream:
    if end punc, break
    parse_atom (will call special parsing if needed, call list if nested) and push to container
  return container if more than 1, else single atom
}

parsing params () {
  // return [0: all params/vars, 1: all values]
  // CAREFUL: can be either ((a 1)) or (a b c)
  // Skip first punc, if punc => (skip punc, first match is param, second is value group, skip end punc) => should be own function? or while loop?  
  // if var => var group
  if punc, skip start punc
  if var, save var name else throw error
  if value, parse atom // This can be thrown away later if not lambda 
  else value = "" to allow for mixing defs with and without values
  if punc, skip end pung if no punc
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
