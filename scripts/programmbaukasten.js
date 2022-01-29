"use strict";

//Language features
const FEATURE_INFIX   = false;  //enable infix arithmetic aka numbers as pseudo-actors
const FEATURE_STREAMS = true; //load the stream example from SICP chapter 3
const FEATURE_ACCOUNT = true; //load the bank account example from SICP chapter 3
const FEATURE_MINIMAL_MATH = false; //implement multiplication as repeated addition etc.
//Fewer primitive operations, at the cost of efficiency

//Editor features
let   DEBUG_TRACE = false; //trace (i.e. log to console) every
//call to eval or apply and every
//returned value. May be toggled at
//run time.  WARNING: Generates a lot
//of output! Test with (+ 1 2) first!
//misc. constants
let DEBUG_TRACE_LEVEL = 0;


//------------------------------
// Parsing related stuff
//------------------------------
function tokenize(inputString){
    const groups = inputString.split(/(\".*?\")/); //split at ".*" strings, but keep them (thanks to the grouping parentheses)
    let tokens = [];
    for (let group of groups) {
	if(group.startsWith('"')){
	    tokens.push(group); //keep strings as they are
	} else {
	    tokens = tokens.concat(tokenizeNonString(group)); //split everything else
	}
    }
    return tokens;
}


function tokenizeNonString(inputString){
    //Insert spaces before and after parentheses, then split at
    //spaces to get a flat array of tokens
    const withSpaces = inputString.
	  replace(/\(/g, " ( ").
	  replace(/\)/g, " ) ").
	  replace(/\'/g, " ' ").
	  replace(/\`/g, " ` ").
	  replace(/\,/g, " , ").
	  replace(/\n/g, " ").
	  replace(/\t/g, " ");
    return withSpaces.split(/\s+/).filter(string=>string.length>0);
}

function* parseAST(tokens) {
    //Transform the flat array from tokenize() into a
    //hierarchical AST
    function parseToken(token) {
	//Parse an individual token.
	if(token===undefined) {
	    throw "Empty input";
	} else if(token === "(") {
	    return parseList();
	} else if(token ==="'") {
	    return {type:"cons",
		    car: {type:"symbol", value: "quote"},
		    cdr: {type: "cons", car:parseToken(tokens.shift()), cdr: NIL}};
	} else if(token ==="`") {
	    return {type:"cons",
		    car: {type:"symbol", value: "quasi-quote"},
		    cdr: {type: "cons", car:parseToken(tokens.shift()), cdr: NIL}};
	} else if(token ===",") {
	    return {type:"cons",
		    car: {type:"symbol", value: "unquote"},
		    cdr: {type: "cons", car:parseToken(tokens.shift()), cdr: NIL}};
	} else if  (token.match(/^-?[0-9]+$/) != null) {
	    return {type: "number", value: parseInt(token)};
	} else if (token.match(/^[a-zA-Z0-9\#\!\$\%\&\*\+\-\.\/\:\<\=\>\?\@\^\_\~]+$/) != null) {
	    return {type: "symbol", value: token};
	} else if (token.match(/^\".*\"$/)) {
	    return {type: "string", value: token.slice(1,-1)};
	} else {
	    //debug
	    for(let char of token) {
		console.log(char.charCodeAt(0));
	    }
	    throw "Invalid token: " + token;
	}
    }
    
    function parseList() {
	//Read tokens until the end of a list and return them as
	//an array.  With the recursive calls and the global
	//token stream this is enough to parse nested list
	//structures
	let list = NIL;
	while(true) {
	    token = tokens.shift();
	    if(token === undefined) {
		throw "Missing ')'";
	    } else if (token === ")") {
		return list;
	    } else if ((/^\s+$/g).test(token)) {
		//console.log("ignored whitespace");
		continue;
	    } else {
		list = append(list, parseToken(token));
	    }
	}
    }
    let token = 0;
    let ast = NIL;
    while(tokens.length > 0) {
	token = tokens.shift();
	if(token === "(") {
	    ast = parseList();
	} else if ((/^\s+$/g).test(token)) {
	    //console.log("ignored whitespace");
	    continue;
	} else {
	    ast = parseToken(token);
	}
	yield ast;
    }
}

//----------------------
// Cons helper functions
//----------------------
function append(list, element) {
    //Appends a given element to a cons-list
    let help = list;
    if(list === NIL) {
	return {type: "cons", car: element, cdr: NIL};
    } else {
	while(help.cdr != NIL) {
	    if(help.type != "cons") {
		console.log(help);
		throw "Append expected a proper list, but found a " + help.type;
	    }
	    help = help.cdr;
	}
	help.cdr = {type: "cons", car: element, cdr: NIL};
	return list;
    }
}

function* generateCons(cons) {
    //Yields each element of a cons-list. Improper lists are
    //supported by a hack.
    if(cons === NIL) {
	return cons;
    } else if (cons.type != "cons") {
	throw "generateCons expected a cons cell, but found a " + cons.type;
    } else {
	while(cons.type === "cons") {
	    yield cons.car;
	    cons = cons.cdr;
	}
	if(cons != NIL) {
	    yield {type: "symbol", value: "."}
	    yield cons;
	}
    }
}

function lengthCons(cons) {
    //Returns the length of a cons-list.
    let n = 0;
    while(true) {
	if(cons === NIL) {
	    return n;
	} else if (cons.type === "cons") {
	    n = n + 1;
	    cons = cons.cdr;
	} else {
	    throw "lengthCons: not a proper list: " + cons.type;
	}
    }
}

function nthCons(cons, n) {
    //Gets the nth value from a list, starting with element 0.
    while(true) {
	if(n <= 0) {
	    return cons.car;
	}
	n = n - 1;
	cons = cons.cdr;
    }
}

//-------------------
// Set Helper methods
//-------------------

function setEqual(as, bs) {
    return (as.size === bs.size) && isSuperset(as,bs);
}

function isSuperset(set, subset) {
    for (let elem of subset) {
        if (!set.has(elem)) {
            return false;
        }
    }
    return true;
}

//---------------
// Syntax objects
//---------------

//syntax objects have the form
//{type:"syntax",expr: <some s-expression>,
//scopes: <a javascript Set of scopes>}

//-------------
// Eval & Apply
//-------------

function lispeval (ast,env) {
    //Evaluatest the expression ast (as computed by parseAST) in
    //the environment env. WARNING! Should only be called via
    //trampoline(["eval",ast,env]);
    
    if(ast.type === "cons") {
	if (ast.car.value === "define") {
	    //this takes care of McCarthy's LABEL rule, as a define'd procedure is
	    //executed in an environment where it's already bound. Much more SICP style.
	    if (ast.cdr.car.type === "symbol") {
		env[ast.cdr.car.value] = trampoline(["eval",ast.cdr.cdr.car,env]);
		//return ["value",env.__get(ast.cdr.car.value)];//return assigned value
		return ["value",ast.cdr.car];//return symbol name
	    } else if (ast.cdr.car.type === "cons") {
		let spec = ast.cdr.car;
		let body = ast.cdr.cdr;
		env[spec.car.value] = {type: "lambda", args: spec.cdr, env: makeEnv(env), body: body};
		//return ["value",env.__get(spec.car.value)];//return assigned lambda expression
		return ["value",spec.car];//return function name
	    } else {
		throw "Define expected a symbol or a cons (i.e. function spec), found " + printExpr(ast.cdr.car);
	    }
	} else if (ast.car.value === "set!") {
	    if (ast.cdr.car.type === "symbol") {
		let targetEnv = env;
		let found = false;
		while (targetEnv != null) {
		    if(targetEnv.__get(ast.cdr.car.value,false) != undefined) {
			found = true;
			targetEnv[ast.cdr.car.value] = trampoline(["eval",ast.cdr.cdr.car,env]);
			return ["value",env.__get(ast.cdr.car.value)];
		    } else {
			targetEnv = targetEnv.__parent;
		    }
		}
		//not found
		throw "Unknown identifier: " + ast.cdr.car.value;
		//permissive variant (set the value in the current environment if not found elsewhere):
		//env[ast.cdr.car.value] = trampoline(["eval",ast.cdr.cdr.car,env]);
	    } else {
		throw "Set! expected a symbol, found " + printExpr(ast.cdr.car);
	    }
	    return ["value",env.__get(ast.cdr.car.value)];
	} else if (ast.car.value === "primitive-set-car!") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type === "cons") {
		a.car = b;
		return ["value",a];
	    } else {
		throw "set-car! expected a cons cell, fond " + printExpr(ast.cdr.car);
	    }
	} else if (ast.car.value === "primitive-set-cdr!") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type === "cons") {
		a.cdr = b;
		return ["value",a];
	    } else {
		throw "set-car! expected a cons cell, fond " + printExpr(ast.cdr.car);
	    }
	} else if (ast.car.value === "primitive-concat") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type != "string") {
		throw "Cannot concat: " + printExpr(a);
	    }
	    if (b.type != "string") {
		throw "Cannot concat: " + printExpr(b);
	    }
	    let result = a.value+b.value;
	    return ["value", {type: "string", value: result}];
	} else if (ast.car.value === "primitive-plus") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type != "number") {
		throw "Cannot add: " + printExpr(a);
	    }
	    if (b.type != "number") {
		throw "Cannot add: " + printExpr(b);
	    }
	    let result = a.value+b.value;
	    return ["value", {type: "number", value: result}];
	} else if (ast.car.value === "primitive-minus") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type != "number") {
		throw "Cannot subtract: " + printExpr(a);
	    }
	    if (b.type != "number") {
		throw "Cannot subtract: " + printExpr(b);
	    }
	    let result = a.value-b.value;
	    return ["value", {type: "number", value: result}];
	} else if (ast.car.value === "primitive-multiply") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type != "number") {
		throw "Cannot multiply: " + printExpr(a);
	    }
	    if (b.type != "number") {
		throw "Cannot multiply: " + printExpr(b);
	    }
	    let result = a.value*b.value;
	    return ["value", {type: "number", value: result}];
	} else if (ast.car.value === "primitive-quotient") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type != "number") {
		throw "Cannot divide: " + printExpr(a);
	    }
	    if (b.type != "number") {
		throw "Cannot divide: " + printExpr(b);
	    }
	    let result = Math.floor(a.value/b.value);
	    return ["value", {type: "number", value: result}];
	} else if (ast.car.value === "primitive-modulo") {
	    let a = trampoline(["eval",ast.cdr.car, env]);
	    let b = trampoline(["eval",ast.cdr.cdr.car, env]);
	    if (a.type != "number") {
		throw "Cannot divide: " + printExpr(a);
	    }
	    if (b.type != "number") {
		throw "Cannot divide: " + printExpr(b);
	    }
	    let result = a.value%b.value;
	    return ["value", {type: "number", value: result}];
	} else if (ast.car.value === "primitive-less-than") {
	    let x = trampoline(["eval",ast.cdr.car, env]).value;
	    let y = trampoline(["eval",ast.cdr.cdr.car, env]).value;
	    if (x != undefined && y != undefined) {
		if(x < y) {
		    return ["value", _T];
		} else {
		    return ["value", _F];
		}
	    } else {
		throw "Cannot compare " + x + " and " + y;
	    }
	} else if (ast.car.value === "quote") {
	    if(ast.cdr.cdr != NIL) {
		throw "Quote expects a single argument. Maybe use a list, e.g. (quote (a b c))?";
	    }
	    return ["value", ast.cdr.car];
	} else if (ast.car.value === "quasi-quote") {
	    return ["value", evalQuasiQuote(ast.cdr.car, env)];
	    //} else if (ast.car.value === "unquote") { //TODO: Is this necessary??? TODO: delete
	    //  if(ast.cdr.cdr != NIL) {
	    //      throw "Quote expects a single argument. Maybe use a list, e.g. (quote (a b c))?";
	    //  }
	    //  return ["value", ast.cdr.car];
	} else if (ast.car.value === "primitive-eval") {
	    if(ast.cdr.cdr != NIL) {
		throw "eval expects a single argument.";
	    }
	    let arg = trampoline(["eval", ast.cdr.car, env]);
	    return ["eval", arg, env];
	} else if (ast.car.value === "primitive-car") {
	    let obj = trampoline(["eval",ast.cdr.car, env]);
	    if (obj === undefined) {
		throw "Undefined object: " + printExpr(ast.cdr.car);
	    }
	    if(obj.type === "cons") {
		return ["value", obj.car];
	    } else {
		throw "Car expected a cons cell, found " + printExpr(obj);
	    }
	} else if (ast.car.value === "primitive-cdr") {
	    let obj = trampoline(["eval",ast.cdr.car, env]);
	    if (obj === undefined) {
		throw "Undefined object: " + printExpr(ast.cdr.car);
	    }
	    if(obj.type === "cons") {
		return ["value", obj.cdr];
	    } else {
		throw "Cdr expected a cons cell, found " + printExpr(obj);
	    }
	} else if (ast.car.value === "primitive-cons") {
	    let first = ast.cdr.car;
	    let rest  = ast.cdr.cdr;
	    return ["value", {type: "cons", car: trampoline(["eval",first,env]), cdr: trampoline(["eval",rest.car,env])}];
	} else if (ast.car.value === "primitive-equal?") {
	    let x = trampoline(["eval",ast.cdr.car,env]);
	    let y = trampoline(["eval",ast.cdr.cdr.car,env]);
	    if(equal(x,y,env)) {
		return ["value", _T];
	    } else {
		return ["value", _F];
	    }
	} else if (ast.car.value === "primitive-eq?") {
	    let x = trampoline(["eval",ast.cdr.car,env]);
	    let y = trampoline(["eval",ast.cdr.cdr.car,env]);
	    if(eq(x,y,env)) {
		return ["value", _T];
	    } else {
		return ["value", _F];
	    }
	    //TODO: remove
	    //} else if (ast.car.value === "primitive-atom?") {
	    //if (trampoline(["eval",ast.cdr.car, env]).type === "cons") {
	    //    return ["value", _F];
	    //} else {
	    //    return ["value", _T];
	    //}
  	} else if (ast.car.value === "primitive-type") {
	    //takes care of McCarthy's atom and RNRS type predicates
	    let val = trampoline(["eval",ast.cdr.car, env]);
	    return ["value", {type:"symbol", value: val.type}];
	} else if (ast.car.value === "primitive-apply") {
	    let fun = trampoline(["eval",ast.cdr.car,env]);
	    let args = env.__get(ast.cdr.cdr.car.value);
	    return ["apply",fun,args,env, ast.cdr.cdr.car.value];
	} else if (ast.car.value === "primitive-js-eval") {
	    if(typeof(ast.cdr.car.value) === "string") {
		let jsExpr = trampoline(["eval", ast.cdr.car, env])
		let jsResult = eval(jsExpr.value) + "";
		
		return ["value", {type:"string", value: jsResult}]; //TODO: change to proper string
	    } else {
		throw "js-eval can only eval strings, found " + typeof(ast.cdr.car.value);
	    }
	} else if (ast.car.value === "cond") {
	    for(let clause of generateCons(ast.cdr)) {
		let condition = undefined;
		if(clause.car.type === "symbol" && clause.car.value === "else") {
		    condition = _T;
		} else {
		    condition = trampoline(["eval",clause.car, env]);
		}
		if(condition.value != "#f") {
		    return ["eval",clause.cdr.car,env];
		    break;
		}
	    }
	    return ["value", NIL];
	} else if (ast.car.value === "lambda") {
	    return ["value", {type: "lambda", args: ast.cdr.car, env: makeEnv(env), body: ast.cdr.cdr}];
	} else if (ast.car.value === "macro") {
	    return ["value", {type: "macro",  args: ast.cdr.car, env: makeEnv(env), body: ast.cdr.cdr}];
	} else if (ast.car.value === "error") {
	    throw printExpr(ast.cdr);
	} else if (ast.car.value === "let") { //TODO: remove/hide once proper macros are implemented
	    let bindings = ast.cdr.car;
	    let body = ast.cdr.cdr;
	    let letEnv = makeEnv(env);
	    for(let binding of generateCons(bindings)) {
		letEnv[binding.car.value] = trampoline(["eval", binding.cdr.car, env]);
	    }
	    let result = null;
	    for(let expression of generateCons(body)) {
		result = trampoline(["eval", expression, letEnv]);
	    }
	    return ["value", result];
	} else {
	    //--------------------
	    //Function application
	    //--------------------
	    let fun = trampoline(["eval",ast.car, env]);
	    if(fun === undefined) {
		throw "unknown function: " + ast.car.value;
	    }
	    //console.log("DEBUG: eval(function)");
	    //console.log(fun);
	    
	    //The following code allows for numbers as functions.
	    //Functions apply their first arg to (cons <themself> <& following args>).
	    //Thus, (fully parenthesized) infix arithmetic can be used.
	    //This is inspired by Gabriel and Steele, Evolution of Lisp
	    //https://www.dreamsongs.com/Files/HOPL2-Uncut.pdf (see page 94)
	    //This feature can be turned off by setting FEATURE_INFIX to false.
	    if(FEATURE_INFIX && fun.type === "number") {
		let args = NIL;
		let newfun = trampoline(["eval",ast.cdr.car, env]);
		if(newfun.type != "lambda" && newfun.type != "macro") {
		    throw "Numbers cannot receive messages of type: " + newfun.type;
		}
		for (let arg of generateCons(ast.cdr.cdr)) {
		    args = append(args,trampoline(["eval",arg,env]));
		}
		return ["apply",newfun,{type: "cons", car: fun, cdr: args}, env, "Actor message " + printExpr(newfun)];
	    }
	    
	    //new feature "macros"
	    if (fun.type === "macro") {
		let args = NIL;
		for (let arg of generateCons(ast.cdr)) {
		    args = append(args,arg); //<-- difference #1
		}
		return ["apply",fun, args, env, "macro " + ast.car.value];
	    }
	    //end "macros"
	    //assume fun to be a lambda, let apply deal with errors //TODO: Fix this comment, it's fake news.
	    if (fun.type === "lambda") {
		
		let args = NIL;
		for (let arg of generateCons(ast.cdr)) {
		    args = append(args,trampoline(["eval",arg,env]));
		}
		return ["apply",fun, args, env, ast.car.value];
	    }
	    throw "Cannot apply type " + fun.type
	}
    } else {
	//-------------------------
	//The expression is an atom
	//-------------------------
	if (ast.type === "number") {
	    return ["value", ast];
	} else if (ast.type === "symbol") {
	    let astValue = env.__get(ast.value);
	    if (astValue === undefined) {
		throw "Unknown symbol: " + printExpr(ast);
	    }
	    return ["value", astValue];
	} else if (ast.type === "string") {
	    return ["value", ast];
	} else {
	    throw "Illegal type: " + ast;
	}
    }
}

function trampoline(packedArgs) {
    //Trampoline calls eval and apply again and again. Both
    //functions either return an actual return value, or
    //information that tells trampoline which function needs to
    //be called next. This turns tail recursion into iteration.
    
    while(true) {
	if(packedArgs[0] === "value") {
	    if(DEBUG_TRACE) {
		console.log(" ".repeat(DEBUG_TRACE_LEVEL) + "=> " + printExpr(packedArgs[1]));
	    }
	    return packedArgs[1];
	} else if (packedArgs[0] === "eval") {
	    if(DEBUG_TRACE) {
		console.log(" ".repeat(DEBUG_TRACE_LEVEL) + "eval " + printExpr(packedArgs[1]));
	    }
	    DEBUG_TRACE_LEVEL += 1;
	    packedArgs = lispeval(packedArgs[1],packedArgs[2]);
	    DEBUG_TRACE_LEVEL -= 1;
	} else if (packedArgs[0] === "apply") {
	    if(DEBUG_TRACE) {
		console.log(" ".repeat(DEBUG_TRACE_LEVEL) + "apply " + (packedArgs[4]||printExpr(packedArgs[1])) + " to " + printExpr(packedArgs[2]));
	    }
	    packedArgs = lispapply(packedArgs[1],packedArgs[2],packedArgs[3],packedArgs[4]);
	} else {
	    throw "illegal return value " + packedArgs +" // "+ printExpr(packedArgs);
	}
    }
}

function lispapply(fun,args,env,name) {
    //Applies the function fun to the arguments in args in the
    //environment env. A function name can be supplied for
    //better error messages.
    
    //Check numer of arguments. Ignore this, if a &rest argument is used
    if(lengthCons(args) != lengthCons(fun.args) && nthCons(fun.args, lengthCons(fun.args)-1).value != "&rest") {
	throw "Wrong number of arguments in "+ name +
	    ". expected " + lengthCons(fun.args) + " got " + lengthCons(args);
    }
    
    //Make a new environment for the function.
    let funEnv = makeEnv(fun.env);
    
    //The magic code that makes macros evaluate their arguments
    //on read.
    if(fun.type === "macro") {
	funEnv["__get"] = function(val,checkparent = true) { //<-- difference #2
	    if(funEnv[val] != undefined) {
		return trampoline(["eval",funEnv[val],env]);
	    } else if (funEnv["__parent"] && checkparent) {
		return funEnv["__parent"].__get(val);
	    } else {
		//throw "Apply: Undefined value: " + val;
	    }
	}
    }
    
    //Put the arguments in the new environment. The arguments
    //have already been evaluated in eval (or not, in the case
    //of a macro).
    let argshelper = fun.args;
    while(argshelper != NIL) {
	if(argshelper.car.type === "symbol" && argshelper.car.value === "&rest") {
	    if(fun.type === "macro") {
		funEnv[argshelper.car.value] = {type:"cons", car: {type:"symbol",value:"list"}, cdr: args};
	    } else {
		funEnv[argshelper.car.value] = args;
	    }
	    break;
	}
	funEnv[argshelper.car.value] = args.car;
	args = args.cdr;
	argshelper = argshelper.cdr;
    }
    let result = NIL;
    
    //Execute each instruction in the functions body. Return
    //before executing the last instruction to enable tail
    //recursion via trampoline.
    let listwalker = fun.body;
    if(listwalker === NIL) { return ["value",NIL];}
    while(true) {
	if(listwalker.cdr === NIL) {
	    //if(DEBUG_TRACE) {
	    //    console.log(" ".repeat(DEBUG_TRACE_LEVEL) + "Tail Call: ");
	    //}
	    return ["eval", listwalker.car, funEnv];
	} else {
  	    result = trampoline(["eval",listwalker.car,funEnv]);
	    listwalker = listwalker.cdr;
	}
    }	  
}

function evalQuasiQuote(ast,env) {
    if(ast === NIL) {
	return NIL ;
    } else if (ast.type === "cons") {
	if(ast.car.type === "symbol" && ast.car.value === "unquote") {
	    if(ast.cdr.cdr != NIL) {
		throw "Unquote expects a single argument. Maybe use a list, e.g. (quote (a b c))?";
	    } else {
		return trampoline(["eval",ast.cdr.car,env]);
	    }
	} else {
	    return {type:"cons", car:evalQuasiQuote(ast.car, env), cdr: evalQuasiQuote(ast.cdr, env)};
	}
    } else {
	return ast;
    }
}

//----------------------------------
// Helper functions for eval & apply
//----------------------------------
function makeEnv (parent) {
    //Create a new environment which will later contain bindings
    //(variable->value). Get a value from the environment env by
    //calling env.get(variable). If a value is not found, the
    //parent environments are checked recursively. WARNING:
    //Macros hack this implementation in lispapply.
    
    let env = {__parent: parent};
    env["__get"] = function(val, checkparent=true) {
	if(env[val] != undefined) {
	    return env[val];
	} else if (env["__parent"] && checkparent) {
	    return env["__parent"].__get(val);
	} else {
	    //throw "MakeEnv: Undefined value: " + val;
	}
    }
    return env;
}

function equal(x,y) {
    if(x.type === "lambda" || x.type === "lambda") {
	throw "don't compare lambdas";
    }
    if(x.type === "cons") {
	return y.type === "cons" &&
	    equal(x.car,y.car) &&
	    equal(x.cdr,y.cdr);
    } else {
	return x.type === y.type && x.value === y.value;
    }
}

function eq(x,y) {
    if(x.type === "lambda" || x.type === "lambda") {
	throw "don't compare lambdas";
    }
    if(x.type === "cons") {
	return y.type === "cons" &&
	    x === y;
    } else {
	return x.type === y.type && x.value === y.value;
    }
}


//---------------
// REPL functions
//---------------


function escapeHtml(unsafe) {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
    //.toUpperCase(); //retro Lisp compatibility mode
}


function readeval (expr, env) {
    //Convert a textual lisp program into an ast and eval it.
    let result = NIL;
    for(let ast of parseAST(tokenize(expr),[])) {
	result =  trampoline(["eval",ast,env]);
    }
    return result;
}

function printExpr(expr) {
    //Prints (i.e. returns as a string) a lisp expression in
    //human readable form.
    if(expr === undefined) {
	return "<no value>";
    }
    
    let result = "";
    
    if(expr.type == "cons") {
	result = "(";
	for(let element of generateCons(expr)) {
	    result += printExpr(element) + " ";
	}
	result = result + ")";	      
    } else if(expr.type === "lambda"){
	let body = printExpr(expr.body);
	result =  "(lambda " + printExpr(expr.args) + " " + body.substr(1,body.length-2) + ")";
    } else if(expr.type === "macro"){
	let body = printExpr(expr.body);
	result =  "(macro " + printExpr(expr.args) + " " + body.substr(1,body.length-2) + ")";
    } else if (expr.type === "string"){
	result =  '"' + expr.value + '"';	      
    } else if (expr.value != undefined){
	result =  expr.value;
    } else {
	result =  "<unprintable " + expr + ">";
    }
    return result.toString().replace(/\s\)/g,")");
}

//-------------------------------
// Global Environment & Constants
//-------------------------------
// We will need a global environment in which top level definitions
//and expressions are evaluated.
let GLOBAL_ENV = makeEnv(null);
//These constants should never be changed. But if you want to
//(define #f #t), feel free to try it...
GLOBAL_ENV["#t"] = {type: "symbol", value: "#t"};
GLOBAL_ENV["#f"] = {type: "symbol", value: "#f"};
GLOBAL_ENV["()"] = {type: "symbol", value: "()"};
//GLOBAL_ENV["lambda"] = {type: "symbol", value: "lambda"};//HACK: probably not a good idea...
//GLOBAL_ENV["quote"] = {type: "symbol", value: "quote"};//probably unneccessary
//Constants to make the javascript code less ugly
const _T = GLOBAL_ENV.__get("#t");
const _F = GLOBAL_ENV.__get("#f");
const NIL = GLOBAL_ENV.__get("()");

//---------------------
// Predefined functions
//---------------------
//The basic interpreter is ready to run now.
//Time to bootstrap a usable environment.
//template for copy&paste: readeval("(define )",GLOBAL_ENV);

//lisp wrappers for all appropriate primitives
readeval("(define (+ &rest) (cond ((equal? () &rest) 0) (#t (primitive-plus (car &rest) (apply + (cdr &rest))))))",GLOBAL_ENV);
readeval("(define (- &rest) (cond ((equal? () &rest) 0) (#t (primitive-minus (car &rest) (apply + (cdr &rest))))))",GLOBAL_ENV);
readeval("(define (< a b) (primitive-less-than a b))",GLOBAL_ENV);
readeval("(define (cons a b) (primitive-cons a b))",GLOBAL_ENV);
readeval("(define (car a) (primitive-car a))",GLOBAL_ENV);
readeval("(define (cdr a) (primitive-cdr a))",GLOBAL_ENV);
readeval("(define (type a) (primitive-type a))",GLOBAL_ENV);
readeval("(define (apply fun args) (primitive-apply fun args))",GLOBAL_ENV);
readeval("(define (equal? a b) (primitive-equal? a b))",GLOBAL_ENV);
readeval("(define (eq? a b) (primitive-eq? a b))",GLOBAL_ENV);
readeval("(define (set-car! a b) (primitive-set-car! a b))",GLOBAL_ENV);
readeval("(define (set-cdr! a b) (primitive-set-cdr! a b))",GLOBAL_ENV);
readeval("(define (eval e) (primitive-eval e))",GLOBAL_ENV);
readeval("(define (js-eval e) (primitive-js-eval e))",GLOBAL_ENV);
readeval('(define (concat &rest) (cond ((equal? () &rest) "") (#t (primitive-concat (car &rest) (apply concat (cdr &rest))))))',GLOBAL_ENV);
//additional primitives are define, set!, quote, cond, lambda, macro, error
//see "misc. arithmetic functions" for even more primitives

//type predicates
readeval("(define (atom? a) (not (equal? (type a) 'cons)))",GLOBAL_ENV);
readeval("(define (boolean? a) (or (equal? a #f) (equal a #t)))",GLOBAL_ENV);
readeval("(define (pair? a) (equal? (type a) 'cons))",GLOBAL_ENV);
readeval("(define (symbol? a) (equal? (type a) 'symbol))",GLOBAL_ENV);
readeval("(define (number? a) (equal? (type a) 'number))",GLOBAL_ENV);
readeval("(define (procedure? a) (equal? (type a) 'lambda))",GLOBAL_ENV);

//some macros and essential definitions
readeval("(define if (macro (i t e) (cond (i t) (#t e))))",GLOBAL_ENV);
readeval("(define and (macro (p q) (cond (p q) (#t #f))))",GLOBAL_ENV);
readeval("(define or (macro (p q) (cond (p p) (#t q))))",GLOBAL_ENV);
readeval("(define (not x) (if (= x #f) #t #f))",GLOBAL_ENV);
readeval("(define (begin &rest) (if (null? &rest) () (if (null? (cdr &rest)) (car &rest) (apply begin (cdr &rest)))))",GLOBAL_ENV);//HACK, depends on the arguments being evaluated in order
readeval("(define delay (macro (exp) (lambda () exp)))",GLOBAL_ENV);
readeval("(define (force delayed-object) (delayed-object))",GLOBAL_ENV);
readeval("(define while (macro (condition &rest) (if condition (begin ((lambda () &rest)) (while condition &rest)) ())))",GLOBAL_ENV);


//c*r functions
readeval("(define (caar x) (car (car x)))",GLOBAL_ENV);
readeval("(define (cadr x) (car (cdr x)))",GLOBAL_ENV);
readeval("(define (cdar x) (cdr (car x)))",GLOBAL_ENV);
readeval("(define (cddr x) (cdr (cdr x)))",GLOBAL_ENV);
readeval("(define (caaar x) (car (car (car x))))",GLOBAL_ENV);
readeval("(define (caadr x) (car (car (cdr x))))",GLOBAL_ENV);
readeval("(define (cadar x) (car (cdr (car x))))",GLOBAL_ENV);
readeval("(define (caddr x) (car (cdr (cdr x))))",GLOBAL_ENV);
readeval("(define (cdaar x) (cdr (car (car x))))",GLOBAL_ENV);
readeval("(define (cdadr x) (cdr (car (cdr x))))",GLOBAL_ENV);
readeval("(define (cddar x) (cdr (cdr (car x))))",GLOBAL_ENV);
readeval("(define (cdddr x) (cdr (cdr (cdr x))))",GLOBAL_ENV);


//misc. arithmetic functions
readeval("(define = equal?)",GLOBAL_ENV);
readeval("(define (> x y) (< y x))",GLOBAL_ENV);
readeval("(define (>= x y) (or (> x y) (equal? x y)))",GLOBAL_ENV);
readeval("(define (<= x y) (or (< x y) (equal? x y)))",GLOBAL_ENV);
if(FEATURE_MINIMAL_MATH === true){
    readeval("(define (* x y) (cond ((equal? y 0) 0) ((< y 1) (* (- 0 x) (- 0 y))) (#t (+ x (* x (- y 1))))))",GLOBAL_ENV);
    readeval("(define (modulo x y) (cond ((equal? x y) 0) ((< x y) x) (#t (modulo (- x y) y))))",GLOBAL_ENV);
    readeval("(define remainder modulo)",GLOBAL_ENV); //TODO: implement sign differences
    readeval("(define (quotient x y)" +
	     "  (define (divhelp x y acc)" +
	     "    (cond ((equal? y 0) (error quotient -- Division by zero))" +
	     "          ((< y 0) (error quotient -- Only positive values for y are supported))" +
	     "          ((< x y)  acc) " +
	     "          ((>= x y) (divhelp (- x y) y (+ acc 1)))))" +
	     "  (divhelp x y 0))",GLOBAL_ENV);
} else {
    readeval("(define (* x y) (primitive-multiply x y))",GLOBAL_ENV);
    readeval("(define (modulo x y) (primitive-modulo x y))",GLOBAL_ENV);
    readeval("(define remainder modulo)",GLOBAL_ENV); //TODO: implement sign differences
    readeval("(define (quotient x y) (primitive-quotient x y))",GLOBAL_ENV);
}	  
readeval("(define (even? x) (= (modulo x 2) 0))",GLOBAL_ENV);
readeval("(define (odd?  x) (= (modulo x 2) 1))",GLOBAL_ENV);
readeval("(define (abs x) (cond ((< x 0) (- 0 x)) (else x)))",GLOBAL_ENV);
readeval("(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))",GLOBAL_ENV);
readeval("(define (lcm a b) (* (abs a) (quotient (abs b) (gcd a b))))",GLOBAL_ENV);



//list functions
readeval("(define (list &rest) &rest)",GLOBAL_ENV);
readeval("(define (map f l) (cond ((equal? l ()) ()) (else (cons (f (car l)) (map f (cdr l))))))", GLOBAL_ENV);
readeval("(define (reduce f l init) (cond ((equal? l ()) init) (else (reduce f (cdr l) (f init (car l))))))", GLOBAL_ENV);
readeval("(define (filter f l) (cond ((equal? l ()) ()) ((f (car l)) (cons (car l) (filter f (cdr l)))) (#t (filter f (cdr l)))))", GLOBAL_ENV);
readeval("(define (drop n l) (cond ((equal? l ()) ()) ((= n 0) l) (else (drop (- n 1) (cdr l)))))", GLOBAL_ENV);
readeval("(define (list-tail l k) (drop k l))", GLOBAL_ENV);
readeval("(define (list-ref l k) (cond ((equal? k 0) (car l))  (else (list-ref (cdr l) (- k 1)))))", GLOBAL_ENV);
readeval("(define (length l) (cond ((equal? l ()) 0)  (#t (+ 1 (length (cdr l))))))", GLOBAL_ENV);
readeval("(define (take n l) (cond ((equal? l ()) ()) ((= n 0) ()) (else (cons (car l) (take (- n 1) (cdr l))))))", GLOBAL_ENV);
readeval("(define (null? l) (equal? l ()))",GLOBAL_ENV);
readeval("(define (assoc obj alist) (cond ((equal? alist ()) ()) ((equal? obj (car (car alist))) (car alist)) (#t (assoc obj (cdr alist)))))",GLOBAL_ENV);
readeval("(define (append-2 l1 l2) (cond ((null? l1) l2) (else (cons (car l1) (append-2 (cdr l1) l2)))))", GLOBAL_ENV);
readeval("(define (append &rest) (cond ((null? &rest) ()) (else (append-2 (car &rest) (apply append (cdr &rest))))))", GLOBAL_ENV);
readeval("(define (reverse xs) (define (revappend xs ys) (cond ((null? xs) ys) (#t (revappend (cdr xs) (cons (car xs) ys))))) (revappend xs ()))", GLOBAL_ENV);//Thank you, Guy Steele!
readeval("(define (member obj l) (cond ((equal? l ()) #f)  (else (if (equal? (car l) obj) #t (member obj (cdr l))))))", GLOBAL_ENV);

//IO functions
//Normally, I would consider using js-eval (instead of defining new primitives) cheating.
//However, the following functions are more about user interface than the language itself,
//so it's kind of okay, I guess...
readeval("(define (load file) (js-eval (concat \"loadFile('\" file \"')\")))",GLOBAL_ENV);
readeval('(define (clear) (js-eval "clrscr();"))',GLOBAL_ENV);
readeval("(define (js-alert string) (js-eval (concat \"alert('\" string \"')\")))",GLOBAL_ENV);

//misc. functions

if(FEATURE_STREAMS) {
    //Streams and delayed evaluation from SICP chapter 3. This
    //feature can be turned off by setting FEATURE_STREAMS to
    //false. 
    readeval("(define cons-stream (macro (a b) (cons a (delay b))))",GLOBAL_ENV);
    readeval("(define (stream-car stream) (car stream))",GLOBAL_ENV);
    readeval("(define (stream-cdr stream) (force (cdr stream)))",GLOBAL_ENV);
    readeval("(define the-empty-stream ())",GLOBAL_ENV);
    readeval("(define stream-null? null?)",GLOBAL_ENV);
    readeval("(define (stream-ref s n) (if (= n 0) (stream-car s) (stream-ref (stream-cdr s) (- n 1))))",GLOBAL_ENV);
    readeval("(define (stream-map proc s) (if (stream-null? s) the-empty-stream (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))",GLOBAL_ENV);
    readeval("(define (stream-for-each proc s)  (if (stream-null? s) (quote done) (begin (proc (stream-car s)) (stream-for-each proc (stream-cdr s)))))",GLOBAL_ENV);
    //skipped display-stream and display-line for now; need to implement display etc.
    readeval("(define (stream-enumerate-interval low high) (if (> low high) the-empty-stream (cons-stream low (stream-enumerate-interval (+ low 1) high))))",GLOBAL_ENV);
    readeval("(define (stream-filter pred stream) (cond ((stream-null? stream) the-empty-stream) ((pred (stream-car stream)) (cons-stream (stream-car stream)"+
	     "(stream-filter pred (stream-cdr stream)))) (else (stream-filter pred (stream-cdr stream)))))",GLOBAL_ENV);
    //TODO: Why does this example fail? (stream-car (stream-cdr (stream-filter even? (stream-enumerate-interval 10000 1000000)))) // ANSWER: Because of the recursion in modulo -.-
    //skipped memo-proc, need to implement set!
    //skipped stuff
    readeval("(define (integers-starting-from n) (cons-stream n (integers-starting-from (+ n 1))))",GLOBAL_ENV);
    readeval("(define integers (integers-starting-from 1))",GLOBAL_ENV);
    readeval("(define (divisible? x y) (= (remainder x y) 0))",GLOBAL_ENV);
    readeval("(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))",GLOBAL_ENV);
    readeval("(define (sieve stream) (cons-stream (stream-car stream) (sieve (stream-filter (lambda (x) (not (divisible? x (stream-car stream)))) (stream-cdr stream)))))",GLOBAL_ENV);
    readeval("(define primes (sieve (integers-starting-from 2)))",GLOBAL_ENV);
}

if(FEATURE_ACCOUNT) {
    //changed eq? to equal, crippeld the error message due to missing backquote support. I should import this again once these features are added.
    readeval("(define (make-account balance)"+
	     "  (define (withdraw amount) (if (>= balance amount) (begin (set! balance (- balance amount)) balance) 'Insufficient-funds))"+
	     "  (define (deposit amount) (set! balance (+ balance amount)) balance)"+
	     "  (define (dispatch m) (cond ((equal? m 'withdraw) withdraw) ((equal? m 'deposit) deposit) "+
	     "    (else (error Unknown request -- MAKE-ACCOUNT))))  dispatch)",GLOBAL_ENV);
}

//------------------------
// Tests and related stuff
//------------------------
function _test (expr, expected) {
    //Check if expr evaluates to expected. Log the result on the
    //console.
    let testresult = null;
    let expectedresult = null;
    let error = "";
    try {
	testresult =  readeval(expr, GLOBAL_ENV);
	expectedresult = readeval(expected, GLOBAL_ENV);
    } catch(e) {
	error = e;
    }
    let message = "???";
    if(error != "") {
	message = "!! // Got an error: " + error;
    } else if(equal(testresult,expectedresult)) {
	message = "OK // " + expr + " => " +  printExpr(testresult);
    } else {
	message = "!! //expected: " + printExpr(expectedresult)  +
	    " found: " +  printExpr(testresult) + " in expression: " + expr;
    }
    console.log(message);
}


function testCases() {
    //This function contains misc. regression tests collected
    //during development.
    
    _test("#t",trampoline(["eval",readeval("#t",GLOBAL_ENV),GLOBAL_ENV]).value); //self-evaluation
    _test("(car (cdr (cons 1 (cons 2 (cons 3 4)))))", "2");
    readeval("(define x 3)",GLOBAL_ENV);
    _test ("(+ x (+ x 4))", "10");
    _test("(equal? (cons 1 (+ 1 2)) (cons (+ 0 1) 3))", "#t");
    _test("(cond ((equal? 1 2) #f)((equal? 1 1) #t))", "#t");
    _test("(((lambda (x) (lambda (y) (+ x y))) 6) 5)", "11");
    _test("(let ((x 3) (y 4)) (set! y 5) y)","5")
    readeval("(define (times-2 x) (+ x x))",GLOBAL_ENV);
    _test("(times-2 3)", "6");
    readeval("(define (cadr x) (car (cdr x)))",GLOBAL_ENV);
    _test("(cadr (cons 1 (cons 2 ())))","2");
    readeval("(define (sum x) (if (equal? x 0) 0 (+ x (sum (- x 1)))))",GLOBAL_ENV); //recursion
    _test("(sum 15)", "120");
    _test("(* 100 100)", "10000"); //using predefined scheme functions
    _test("(quotient 250 25)", "10");
    readeval("(define (fib n) (cond ((< n 2) n) (#t (+ (fib (- n 1)) (fib (- n 2))))))",GLOBAL_ENV); //recursive fibonacci:
    _test("(fib 7)","13");
    _test("(procedure? fib)","#t");
    _test("(atom? fib)","#t");
    _test("(number? (fib 3))","#t");
    _test("(symbol? 'fib)","#t");
    _test("(atom? '(fib))","#f");
    _test("(equal? (quote (a b c)) (quote (a b c)))","#t");
    _test("(equal? (quote (a b c)) (quote (a b)))","#f");
    _test("(equal? (map fib (quote (1 2 3))) (quote (1 1 2)))","#t");//yay, quote works
    //The Big One. Factorial using the "poor man's y combinator" (W. Byrd):
    _test("(((lambda (!) (lambda (n) ((! !) n))) (lambda (!) (lambda (n) (cond ((equal? n 0) 1) (#t (* n ((! !) (- n 1)))))))) 5)","120");
    _test("(map atom? (quote (1 (2 3) hi (bye))))","(list #t #f #t #f)");
    _test("(reduce + (list 1 2 3 4 5) 0)","15");
    _test("(if #t 1 (quotient 1 0))","1"); //this would produce an error without macros
    _test("((lambda () (define x (cons 1 ())) (set-cdr! x x) (take 20 x)))","'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)"); //test set-cdr and circular list structures

    if(FEATURE_INFIX === true) {
	readeval("(define (fib n) (define a 0) (define b 1) (while (n > 0) (define help b) (set! b (a + b)) (set! a help) (set! n (n - 1))) a)",GLOBAL_ENV);
	_test("(map fib '(0 1 2 3 4 5 6 7 8 9))","'(0 1 1 2 3 5 8 13 21 34)"); //imperative fibonnaci, relies on infix feature
    } else {
	console.log("   // FEATURE_INFIX is not active, skipping FEATURE_INFIX tests");
    }

    if(FEATURE_ACCOUNT === true) {
	_test("(((make-account 100) 'withdraw) 50)","50");
    }
    
    if(FEATURE_MINIMAL_MATH === true) {
	console.log("   // FEATURE_MINIMAL_MATH is active, skipping problematic tests");
    } else {
	_test("(quotient 100000 2)","50000");
    }

    if (FEATURE_STREAMS === true && FEATURE_MINIMAL_MATH === false) {
	//stream tests include deep recursion, only test when FEATURE_MINIMAL_MATH is inactive
	_test("(stream-ref primes 50)","233");
	_test("(stream-ref no-sevens 100)","117");
	//stress tests tail recursion
	_test("(stream-car (stream-cdr (stream-filter even? (stream-enumerate-interval 10000 1000000))))","10002");
    } else {
	console.log("   // FEATURE_STREAMS is not active (or FEATURE_MINIMAL_MATH active), skipping FEATURE_INFIX tests");
    }

    
}

//--------------------------
//node.js related functions
//--------------------------
function nodeREPL() {
    const { question } = require('readline-sync');

    console.log("der programmbaukasten.");
    console.log("To quit, type 'quit'.");

    let input = '';
    while(true) {
	input = question("scheme> ");
	if(input === "quit" || input === "'quit'") {
	    break;
	}
	console.log(printExpr(readeval(input, GLOBAL_ENV)));
    }
}
