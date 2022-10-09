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

  function unexpected() {
    tokenStream.croak(`Unexpected token: "${JSON.stringify(tokenStream.peek())}"`)
  }

  // ------------- This function processes tokens that are inside of a container, putting them together in a list. 
  // There's the possibilty of adding a separator (in js commas and semicolons), but there's no need in lisp
  // refer to https://lisperator.net/pltut/parser/the-parser
  function delimited(start, stop, parser) {
    let content = []
    if (start) skip_punc(start)  // Useful both for calls and prog
    while (!tokenStream.eof()) {
      if (is_punc(stop)) break // stop if the next token closes the contanier
      content.push(parser())
    }
    skip_punc(stop)
    return content
  }

  function parse_prog() {
    let prog = delimited("(", ")", parse_expression)
    // TODO: check for keyword, lambda or operation?
    if (prog.length == 0) return FALSE
    else if (prog.length == 1) return prog[0]
    else return {type:"prog", prog: prog}
  }

  function parse_binary() {  // SIDE-EFFECT ONLY
    // In the tutorial the function is "maybe binary", but in lisp there is no need for precedence
    // and operators are just keywords, so they must be in the beginning of a prog, making it easier to check for them.
    let tok = is_op()
    if (tok) {
      let first = parse_atom()  // FIXME : check after defining parse atom and parse expression
      let second = parse_atom()  // FIXME
      return {
        type: "binary",
        operator: tok.value,
        left: first,
        right: second
      }
    }
    tokenStream.croak("The operation char wasn't recognized during parsing")
  }

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
  }

  function parse_atom()
  function parse_expression()

  function parse_if() // account for skip_then 
  function parse_bool()
}