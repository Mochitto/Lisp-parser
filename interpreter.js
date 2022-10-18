function Environment(parent_env=null) {
    // Extend the vars from the parent's vars: local scope
    this.vars = Object.create(parent_env ? parent_env.vars: null)
    this.parent = parent_env
    this.get = (name) => {
            if (name in this.vars) return this.vars[name]
            else throw new Error(`Undefined variable ${name}`)
        }
    this.def = (name, value) => {
            this.vars[name] = value
            return name
        } 
    this.extend = () => {
            return new Environment(this)
        }

        // You can def globals/dynamic vars from nested scopes;
        // Should have a def global function? There is no assign!
        // Let can overwrite global vars (creating a local scope)
        // and if functions use that global var in the local scope, the local scope's value
        // should be used instead (should be fixed by creating new scopes when entering a nested sexp)  
    }

function evaluate(node, scope) {
    if (Array.isArray(node)) {
        switch (node[0].type) {
            case "call": 
                let function_to_use = scope.get(node[0].name)
                let arguments = node.slice(1).map((singleNode) => {return evaluate(singleNode, scope)})
                if (function_to_use.length != 0 && function_to_use.length != arguments.length) {
                    throw new Error(`${node[0].name} takes ${function_to_use.length} arguments, but ${arguments.length} were given.`)
                } 
                return function_to_use.apply(null, arguments)
        }
    }
    switch (node.type) {
        case "num":
        case "str":
        case "bool":
            return node.value
        case "var":
            return scope.get(node.value)
        case "def":
            if (node.kind == "var") {
                return scope.def(node.name, evaluate(node.value))
            } else if (node.kind == "func") {
                throw new Error("Function definitions are not supported yet")
                // let newFunction = new Function(
                //     node.params.map((node) => evaluate(node)), 
                //     evaluate(node.body))

                // scope.def(node.name, newFunction)
                // TODO: process the function and store it (so it can be called)
                // TODO: Throw error if the arguments are less than the parameters
                // I think this needs a compiler..? 
                // since body of the function must be in javascript...
            } else {throw new Error(`Unknown definition: ${node}`)}
        case "binary":
            return apply_operator(
                node.operator,
                node.left,
                node.right
            )
        case "if":
            let condition = evaluate(node.cond, scope)
            if (condition != false) return evaluate(node.then, scope) 
            else return node.else ? evaluate(node.else, scope) : false
        case "root":
            return evaluate(node.tree, scope)
        default:

            break // understand when arrays are made: is it part of call?
    }
}

function apply_operator(operator, left, right) {
        console.log(left, operator, right) // { type: 'num', value: 1 } * { type: 'num', value: 2 }
    }


module.exports = [evaluate, Environment]


/* 
eval(exp, env) {
    switch (exp.type) { // TODO: how to access the tree? add a function to enter it?
        case "num":
        case "str":
        case "boolean":
            return exp.value
        case "var":
            return env.get(exp.value)
        case "def":
            if (exp.name) var
            if (exp.name) func
        case "binary"
            return apply_op(exp.value)
        case "call":
            func = eval (exp.func, env) // Should get the function from env
            if (exp.name) lambda make_lambda // Will then default to a normal call
            else: func.apply(null, exp.args.map(arg => eval(arg, env))) // Eval the content of args to apply the function to simple atoms
            // The idea is to create the function () and then apply it to the args
        case "if":
            evaluate cond
            evaluate then if cond true, else eval else (defaults to false)
        case "prog" // Is this when I find arrays? Understand when there are arrays from the AST; this could be needed for the functions' bodies and the AST tree
            let val
            exp.prog.forEach(exp => val = eval(exp, env))     
            return val
    }

    apply op { // I need to eval if I find a list: prog case?
        num: // type validation
            if list: eval prog?
            throw error if not num type or list
            else return val
        
        div:
            throw error if zero division
            else return val

        switch (op)
            case - return num(left) + num(right)
            case + ...
            case ...
            default:
                error can't apply the operator: X
    }

    make_lambda {
        return function {  // can be called like a normal function
            let names = exp.params
            let args = exp.args..?
            let scope = env.extend() // this makes it possible to have local vars separated from global ones
            for (i, i++)
                scope.def(names[i], args[i])
            return eval(exp.body, scope)
        }
    }
}

Environment object (parent) {
    this vars = Objectcreate (parent? parent vars:  null)
    this parent = parent

    env.prot = {
    extend: // This is used for local vars, for let and lambda 
    return a new env with this as parent

    lookup: name // This is used to understand in what scope there is the variable, starting from current
    scope = this 
    while scope // when no scope it means it's the global scope
        if object.hasOwn(scope.vars, name) // [0]: object [1]: property
            return scope
        scope = scope.parent    
    // if nothing is found, it returns null, so false I guess

    get: name // Get's the value of the variable
    if var.name in this.vars
        return this/vars[name]
    throw new Error(Undefined variable)

    set: name, var // Update value of variable. Need to understand this better
    scope = this.lookup(name)
    if (!scope && this.parent) // var non definita e in local-env, used to avoid global def from local

    def: name, value // def new var
    this.vars[name] = value
        

}   
*/

// NOTICE: when you write multiple sexpression in clisp, they get eval one by one; when eval the ast tree, eval and return every part one by one with a for loop
// TODO: make anki cards for the switch/multiple cases
// TODO: make anki cards for the new 
// TODO: write operations tests in a way that if you pass an arg to the terminal call (ex. npm test -- interp +) only the + test runs (for interviewing)