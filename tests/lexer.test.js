const lexer = require("../lexer")

let a = lexer("hello")

test("Testing lexer", () => {
    expect(a.next()).toBe("h");
    expect(a.peek()).toBe("e");
    expect(a.next()).toBe("e");
    expect(a.eof()).toBe(false);
    expect(a.next()).toBe("l");
    expect(a.next()).toBe("l");
    expect(a.next()).toBe("o");
    expect(a.eof()).toBe(true);
})