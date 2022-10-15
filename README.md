# Lisp parser
An exercise built for Recurse center's application process

## Objectives:
- Understanding the different parts of parsing a language
- Building a parser from scratch
- Making most basic features of Common Lisp available

---

### Building Diary
*The building process behind the lexer and tokenizer hasn't been documented*

**07/10/2022**
I spent most of the time reading the guide and trying to understand the main logic that stands behind the parsing part.
As for the lexer and tokenizer, the process seems simple, but translating the process into machine code seems to be quite a challenge, at least for the first time.

**08/10/2022**
Today I was able to build half of the parser.
I feel as if my understanding of recursive functions is getting stronger; I'm also very happy about how I was able to bend the guide to my needs, modifiying the examples to accomodate for Lisp's syntax.

**09/10/2022**
Got closer to completing the parser :)
Spent most of the time reading to understand how to modify the guide.

**10/10/2022**
The parser is now missing just if statements, booleans and lambdas!

**11/10/2022**
I was able to get everything to work, but the code needs some serious refactoring :')
I also still need to implement variables and functions definitions.

**12/10/2022**
Went down a complete refactoring; spent most of the time rewriting the logic behind the parser, this time by myself.
The guide was awesome, but starting over, with the thought of parsing lisp (instead of modifying the grammar given in the guide) feels much better.

**13/10/2022**
Rewrote the parser for the most part. There is a bug that I don't understand related to the lambda function.

**14/10/2022**
The lambda bug prooved to be much harder to understand than any other bug I met before. In the end, the wisest thing was just using... The debugger tool :')
It's now fixed and ready to move to testing and writing the interpreter.

**15/10/2022**
- I've read on how to build an interpreter, and I think I should be able to write a part of it soon!
- Written some tests, even if there are still many corner cases!
I hope to be able to send the project tonight or tomorrow morning :)