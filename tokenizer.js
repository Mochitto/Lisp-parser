function tokenizer(stream) {
    return {
        peek: peek,
        next: next,
        eof: eof,
        croak: stream.croak
    }

    function read_next() {
        read_while(is_whitespace);
        if (stream.eof()) return null;
        var char = stream.peek();
        if (char == ";") { // Missing the multi-line support
            skip_comment()
            return read_next()
        } 
    }
}

export {tokenizer}