/* 
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
*/

// NOTICE: when you write multiple sexpression in clisp, they get eval one by one; when eval the ast tree, eval and return every part one by one with a for loop
// TODO: make anki cards for the switch/multiple cases
// TODO: make anki cards for the new 
// TODO: write operations tests in a way that if you pass an arg to the terminal call (ex. npm test -- interp +) only the + test runs (for interviewing)