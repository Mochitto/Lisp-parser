function tokenizer(stream) {
    let current = null
    const keywords = [
        "and", "or", "not", "t", "T", "NIL", "nil", "max", 
        "min", "format", "first", "list", "then", "if", "lambda"]
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
        else if (char == '"') return read_string()
        else if (is_digit(char)) return read_number()
        else if (is_id_start(char)) return read_ident()
        else if (is_punc(char)) return {type: "punc", value: stream.next()}
        else if (is_op_char(char)) return {type: "op", value: read_while(is_op_char)}
        stream.croak()
    }

    function read_while(predicate) {
        let str = ""
        while (!stream.eof() && predicate(stream.peek())) {
            str += stream.next()
        }
        return str
    }

    function is_digit(char) {
        return /\d/.test(char)
    };

    function is_id_start(char) {
        return /[a-z_]/i.test(char)
    }

    function is_op_char(char) {
        return /[+-/\*=<>]/.test(char)
    }

    function is_punc(char) {
        return /[()]/.test(char)
    }

    function is_whitespace(char) {
        return /\s/.test(char)
    }

    function is_id(char) { // You can also add extra chars here vvvvvvvvvvvv
        return is_id_start(char) // || "other characters".indexOf(char) >= 0
    }

    function is_keyword(keyword) {
        return keywords.includes(keyword)
    }

    function read_number(char) {
        let has_dot = false
        let number = read_while((char) => {
            if (char == ".") {
                if (has_dot) return false;
                has_dot = true;
                return true;
            }
            return is_digit(char)
        })
        return {type: "num", value: parseFloat(number)}
    }

    function read_ident() {
        let id = read_while(is_id) 
        return {type: is_keyword(id) ? "kw" : "var", value: id}
    }

    function read_escaped(special_char) {
        let escaped = false, str = ""
        stream.next()
        while (!stream.eof()) {
            let char = stream.next()
            if (escaped) {
                str += char
                escaped = false
            } else if (char == "\\") {
                escaped = true
            } else if (char == special_char) {
                break
            } else {
                str += char
            }
        }
        return str
    }

    function read_string() {
        return {type: "str", value: read_escaped('"')}
    }

    function skip_comment() {
        read_while((char) => char != "\n")
        stream.next();
    }

    function peek() {
        return current || (current = read_next())
    }

    function next() {
        let token = current;
        current = null;
        return token || read_next() 
    }

    function eof() {
        return peek() == null
    }

}

module.exports = tokenizer
