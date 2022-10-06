function lexer(string) {

    let offset = 0
    let col = 0
    let line = 0
    
    return {
        next: next,
        peek: peek,
        eof: eof,
        croak: croak
    }

    function peek() {return string.charAt(offset)}
    function eof() {return peek() == ""}
    function croak(message) {throw new Error(`${message} (${line}:${col})`)}
    function next() {
        let char = string.charAt(offset++)
        if (char == "\n") {col = 0; line++} else {col++}
        return char
    }
} 

module.exports = lexer