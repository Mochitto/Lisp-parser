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
  function delimited(start, stop, praser=parse_atom) {
    let type = null
    let content = []

    skip_punc(start)
    let first

    // First atom must be a keyword, function or operation
    if (is_keyword()) {first = tokenStream.next(); type = "function"} 
    else if (is_op()) {first = tokenStream.next(); type = "binary"}
    else tokenStream.croak(`${start} is not a function nor an operation`)

    // Get the arguments of the keyword/function/operation
    while (!tokenStream.eof()) {
      if (is_punc(stop)) break // stop if the next token closes the contanier
      content.push(praser())
    }
    skip_punc(stop)

    // Return either a function type or a binary type
    if (type == "function") return {type: "function", name: first.value, args: content}
    else if (type == "binary") {
      if (content.lenght > 2) {console.log(content.length); tokenStream.croak("Too many arguments for binary operation")}
      return {type: "binary", operator: first.value, left: content[0], right: content[1]}
    }
  }

  function parse_expression() {
    let prog = delimited("(", ")")
    if (prog.length == 0) return FALSE
    else return prog
  }

  function parse_atom() {
    if (is_punc("(")) return parse_expression()
    else {
      let tok = tokenStream.next()
      if (tok.type == "var" || tok.type == "num" || tok.type == "str") {
        return tok
      } else unexpected(tok)
    }
  }

  function parse_if() {}
  function parse_bool() {}

function parse_varname() {
  let name = tokenStream.next()
  if (name.type != "var") tokenStream.croak(`Expected variable name but got ${JSON.stringify(name)}`) 
  return name.value
}

function parse_lambda() {
  return {
    type: "call",
    args: delimited("", ")", parse_varname),
    body: parse_expression()
  }
}}

module.exports = lisp_parser
