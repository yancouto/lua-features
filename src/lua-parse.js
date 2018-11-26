// @flow

import * as AST from "./ast-types";
import { errors, raise, unexpected } from "./errors";
import { tokenize } from "./lua-tokenize";

const EOF = 1;
const StringLiteral = 2;
const Keyword = 4;
const Identifier = 8;
const NumericLiteral = 16;
const Punctuator = 32;
const BooleanLiteral = 64;
const NilLiteral = 128;
const VarargLiteral = 256;

export type LuaParseOptions = {|
	// Store comments as an array in the chunk object.
	+comments?: boolean,
	// Store location information on each syntax node as
	// `loc: { start: { line, column }, end: { line, column } }`.
	+locations?: boolean,
	// Store the start and end character locations on each syntax node as
	// `range: [start, end]`.
	+ranges?: boolean,
	// Whether to allow code points outside the Basic Latin block in identifiers
	+extendedIdentifiers?: boolean,
	// The version of Lua targeted by the parser
	+luaVersion?: "5.1" | "5.2" | "5.3" | "LuaJIT",
|};

let options, features;
// Options can be set either globally on the parser object through
// defaultOptions, or during the parse call.
const defaultOptions = {
	comments: true,
	locations: false,
	ranges: false,
	luaVersion: "5.1",
	extendedIdentifiers: false,
};

// ### Abstract Syntax Tree
//
// The default AST structure is inspired by the Mozilla Parser API but can
// easily be customized by overriding these functions.

const ast = {
	labelStatement(label: string): AST.LabelStatement {
		return {
			type: "LabelStatement",
			label,
		};
	},

	breakStatement(): AST.BreakStatement {
		return {
			type: "BreakStatement",
		};
	},

	gotoStatement(label: string): AST.GotoStatement {
		return {
			type: "GotoStatement",
			label,
		};
	},

	returnStatement(args: any): AST.ReturnStatement {
		return {
			type: "ReturnStatement",
			args,
		};
	},

	ifStatement(clauses: any): AST.IfStatement {
		return {
			type: "IfStatement",
			clauses,
		};
	},

	ifClause(condition: any, body: any): AST.IfClause {
		return {
			type: "IfClause",
			condition,
			body,
		};
	},

	elseifClause(condition: any, body: any): AST.ElseifClause {
		return {
			type: "ElseifClause",
			condition,
			body,
		};
	},

	elseClause(body: any): AST.ElseClause {
		return {
			type: "ElseClause",
			body,
		};
	},

	whileStatement(condition: any, body: any): AST.WhileStatement {
		return {
			type: "WhileStatement",
			condition,
			body,
		};
	},

	doStatement(body: any): AST.DoStatement {
		return {
			type: "DoStatement",
			body,
		};
	},

	repeatStatement(condition: any, body: any): AST.RepeatStatement {
		return {
			type: "RepeatStatement",
			condition,
			body,
		};
	},

	localStatement(variables: any, typeList: any, init: any): AST.LocalStatement {
		return {
			type: "LocalStatement",
			variables,
			typeList,
			init,
		};
	},

	assignmentStatement(variables: any, init: any): AST.AssignmentStatement {
		return {
			type: "AssignmentStatement",
			variables,
			init,
		};
	},

	callStatement(expression: any): AST.CallStatement {
		return {
			type: "CallStatement",
			expression,
		};
	},

	functionStatement(
		identifier: any,
		parameters: any,
		parameter_types: any,
		return_types: any,
		hasVarargs: any,
		isLocal: any,
		body: any
	): AST.FunctionDeclaration {
		return {
			type: "FunctionDeclaration",
			identifier,
			isLocal,
			parameters,
			parameter_types,
			return_types,
			hasVarargs,
			body,
		};
	},

	forNumericStatement(
		variable: any,
		start: any,
		end: any,
		step: any,
		body: any
	): AST.ForNumericStatement {
		return {
			type: "ForNumericStatement",
			variable,
			start,
			end,
			step,
			body,
		};
	},

	forGenericStatement(
		variables: any,
		iterators: any,
		body: any
	): AST.ForGenericStatement {
		return {
			type: "ForGenericStatement",
			variables,
			iterators,
			body,
		};
	},

	chunk(body: any): AST.Chunk {
		return {
			type: "Chunk",
			body,
		};
	},

	identifier(name: any): AST.Identifier {
		return {
			type: "Identifier",
			name,
		};
	},

	functionType(parameters: any, returns: any): AST.FunctionType {
		return {
			type: "FunctionType",
			parameter_types: parameters,
			return_types: returns,
		};
	},

	tableType(typeMap: any): AST.TableType {
		return {
			type: "TableType",
			typeMap,
		};
	},

	simpleType(value: any): AST.SimpleType {
		return {
			type: "SimpleType",
			value,
		};
	},

	typeInfo(possibleTypes: any): AST.TypeInfo {
		return {
			type: "TypeInfo",
			possibleTypes,
		};
	},

	typeList(list: any, rest: any): AST.TypeList {
		return {
			type: "TypeList",
			list,
			rest,
		};
	},

	literal(type: any, value: any, raw: any): AST.Literal {
		const type_str =
			type === StringLiteral
				? "StringLiteral"
				: type === NumericLiteral
				? "NumericLiteral"
				: type === BooleanLiteral
				? "BooleanLiteral"
				: type === NilLiteral
				? "NilLiteral"
				: "VarargLiteral";

		return ({
			type: type_str,
			value,
			raw,
		}: any);
	},

	parenthesisExpression(expression: any): AST.ParenthesisExpression {
		return {
			type: "ParenthesisExpression",
			expression,
		};
	},

	tableKey(key: any, value: any): AST.TableKey {
		return {
			type: "TableKey",
			key,
			value,
		};
	},

	tableKeyString(key: any, value: any): AST.TableKeyString {
		return {
			type: "TableKeyString",
			key,
			value,
		};
	},

	tableValue(value: any): AST.TableValue {
		return {
			type: "TableValue",
			value,
		};
	},

	tableConstructorExpression(fields: any): AST.TableConstructorExpression {
		return {
			type: "TableConstructorExpression",
			fields,
		};
	},

	binaryExpression(
		operator: any,
		left: any,
		right: any
	): AST.BinaryExpression | AST.LogicalExpression {
		const type =
			"and" === operator || "or" === operator
				? "LogicalExpression"
				: "BinaryExpression";

		return ({
			type,
			operator,
			left,
			right,
		}: any);
	},
	unaryExpression(operator: any, argument: any): AST.UnaryExpression {
		return {
			type: "UnaryExpression",
			operator,
			argument,
		};
	},
	memberExpression(
		base: any,
		indexer: any,
		identifier: any
	): AST.MemberExpression {
		return {
			type: "MemberExpression",
			indexer,
			identifier,
			base,
		};
	},

	indexExpression(base: any, index: any): AST.IndexExpression {
		return {
			type: "IndexExpression",
			base,
			index,
		};
	},

	callExpression(base: any, args: any): AST.CallExpression {
		return {
			type: "CallExpression",
			base,
			args,
		};
	},

	tableCallExpression(base: any, args: any): AST.TableCallExpression {
		return {
			type: "TableCallExpression",
			base,
			args: [args],
		};
	},

	stringCallExpression(base: any, argument: any): AST.StringCallExpression {
		return {
			type: "StringCallExpression",
			base,
			args: [argument],
		};
	},

	comment(value: any, raw: any) {
		return {
			type: "Comment",
			value,
			raw,
		};
	},
};

const nil_type = ast.typeInfo(new Set([ast.simpleType("nil")]));
const any_type = ast.typeInfo(new Set([ast.simpleType("any")]));

// Wrap up the node object.

function finishNode(node: any) {
	// Pop a `Marker` off the location-array and attach its location data.
	if (trackLocations) {
		const location = locations.pop();
		location.complete();
		location.bless(node);
	}
	return node;
}

// Helpers
// -------

const indexOf = function indexOf(array, element) {
	for (let i = 0, length = array.length; i < length; ++i) {
		if (array[i] === element) return i;
	}
	return -1;
};

// Iterate through an array of objects and return the index of an object
// with a matching property.

function indexOfObject(array, property, element) {
	for (let i = 0, length = array.length; i < length; ++i) {
		if (array[i][property] === element) return i;
	}
	return -1;
}

// Returns a new object with the properties from all objectes passed as
// arguments. Last argument takes precedence.
//
// Example:
//
//     this.options = extend(options, { output: false });

function extend(...args: Array<any>) {
	const dest = {};
	let src;
	let prop;

	for (let i = 0, length = args.length; i < length; ++i) {
		src = args[i];
		for (prop in src)
			if (src.hasOwnProperty(prop)) {
				dest[prop] = src[prop];
			}
	}
	return dest;
}

// #### Raise an throw unexpected token error.
//
// Example:
//
//     // expected <name> near '0'
//     throw raiseUnexpectedToken('<name>', token);

function raiseUnexpectedToken(type: any, token: any) {
	throw raise(token, errors.expectedToken, type, token.value);
}

let token: any;
let previousToken: any;
let lookahead;
let comments;

// ## Lex functions and helpers.

// Read the next token.
//
// This is actually done by setting the current token to the lookahead and
// reading in the new lookahead token.

function next() {
	previousToken = token;
	token = lookahead;
	lookahead = lex();
}

// Consume a token if its value matches. Once consumed or not, return the
// success of the operation.

function consume(value) {
	if (value === token.value) {
		next();
		return true;
	}
	return false;
}

// Expect the next token value to match. If not, throw an exception.

function expect(value) {
	if (value === token.value) next();
	else throw raise(token, errors.expected, value, token.value);
}

function isUnary(token) {
	if (Punctuator === token.type) return "#-~".indexOf(token.value) >= 0;
	if (Keyword === token.type) return "not" === token.value;
	return false;
}

// @TODO this needs to be rethought.
function isCallExpression(expression) {
	switch (expression.type) {
		case "CallExpression":
		case "TableCallExpression":
		case "StringCallExpression":
			return true;
	}
	return false;
}

// Check if the token syntactically closes a block.

function isBlockFollow(token) {
	if (EOF === token.type) return true;
	if (Keyword !== token.type) return false;
	switch (token.value) {
		case "else":
		case "elseif":
		case "end":
		case "until":
			return true;
		default:
			return false;
	}
}

// Scope
// -----

// Store each block scope as a an array of identifier names. Each scope is
// stored in an FILO-array.
let scopes,
	function_scope,
	// The current scope index
	scopeDepth,
	// A list of all global identifier nodes.
	globals;

// Create a new scope inheriting all declarations from the previous scope.
function createScope(isFunction) {
	scopeDepth++;
	scopes.push([]);
	function_scope.push(isFunction);
}

// Exit and remove the current scope.
function destroyScope() {
	scopes.pop();
	function_scope.pop();
	scopeDepth--;
}

// Add identifier name to the current scope if it doesnt already exist.
function scopeIdentifierName(name) {
	if (-1 !== indexOf(scopes[scopeDepth], name)) return;
	scopes[scopeDepth].push(name);
}

// Add identifier to the current scope
function scopeIdentifier(node) {
	scopeIdentifierName(node.name);
	attachScope(node, true);
}

// Attach scope information to node. If the node is global, store it in the
// globals array so we can return the information to the user.
function attachScope(node, isLocal) {
	if (!isLocal && -1 === indexOfObject(globals, "name", node.name))
		globals.push(node);

	node.isLocal = isLocal;
}

// Is the identifier name available in this scope.
function scopeHasName(name) {
	// TODO: simplify this, since it always looks for ...
	for (let i = scopeDepth; i >= 0; i--) {
		if (-1 !== indexOf(scopes[i], name)) return true;
		if (function_scope[i]) break;
	}
	return false;
}

// Location tracking
// -----------------
//
// Locations are stored in FILO-array as a `Marker` object consisting of both
// `loc` and `range` data. Once a `Marker` is popped off the list an end
// location is added and the data is attached to a syntax node.

let locations: Array<any> = [],
	trackLocations: any;

function createLocationMarker() {
	return new Marker(token);
}

function Marker(token) {
	if (options.locations) {
		this.loc = {
			start: {
				line: token.line,
				column: token.range[0] - token.lineStart,
			},
			end: {
				line: 0,
				column: 0,
			},
		};
	}
	if (options.ranges) this.range = [token.range[0], 0];
}

// Complete the location data stored in the `Marker` by adding the location
// of the *previous token* as an end location.
Marker.prototype.complete = function() {
	if (options.locations) {
		this.loc.end.line = previousToken.lastLine || previousToken.line;
		this.loc.end.column =
			previousToken.range[1] -
			(previousToken.lastLineStart || previousToken.lineStart);
	}
	if (options.ranges) {
		this.range[1] = previousToken.range[1];
	}
};

Marker.prototype.bless = function(node) {
	if (this.loc) {
		const loc = this.loc;
		node.loc = {
			start: {
				line: loc.start.line,
				column: loc.start.column,
			},
			end: {
				line: loc.end.line,
				column: loc.end.column,
			},
		};
	}
	if (this.range) {
		node.range = [this.range[0], this.range[1]];
	}
};

// Create a new `Marker` and add it to the FILO-array.
function markLocation() {
	if (trackLocations) locations.push(createLocationMarker());
}

// Push an arbitrary `Marker` object onto the FILO-array.
function pushLocation(marker) {
	if (trackLocations) locations.push(marker);
}

// Parse functions
// ---------------

// Chunk is the main program object. Syntactically it's the same as a block.
//
//     chunk ::= block

function parseChunk() {
	next();
	markLocation();
	createScope(true);
	scopeIdentifierName("...");
	const body = parseBlock();
	destroyScope();
	if (EOF !== token.type) throw unexpected(token);
	// If the body is empty no previousToken exists when finishNode runs.
	if (trackLocations && !body.length) previousToken = token;
	return finishNode(ast.chunk(body));
}

// A block contains a list of statements with an optional return statement
// as its last statement.
//
//     block ::= {stat} [retstat]

function parseBlock() {
	const block = [];
	let statement;

	while (!isBlockFollow(token)) {
		// Return has to be the last statement in a block.
		if ("return" === token.value) {
			block.push(parseStatement());
			break;
		}
		statement = parseStatement();
		consume(";");
		// Statements are only added if they are returned, this allows us to
		// ignore some statements, such as EmptyStatement.
		if (statement) block.push(statement);
	}

	// Doesn't really need an ast node
	return block;
}

// There are two types of statements, simple and compound.
//
//     statement ::= break | goto | do | while | repeat | return
//          | if | for | function | local | label | assignment
//          | functioncall | ';'

function parseStatement() {
	markLocation();
	if (Keyword === token.type) {
		switch (token.value) {
			case "local":
				next();
				return parseLocalStatement();
			case "if":
				next();
				return parseIfStatement();
			case "return":
				next();
				return parseReturnStatement();
			case "function": {
				next();
				const name = parseFunctionName();
				return parseFunctionDeclaration(name);
			}
			case "while":
				next();
				return parseWhileStatement();
			case "for":
				next();
				return parseForStatement();
			case "repeat":
				next();
				return parseRepeatStatement();
			case "break":
				next();
				return parseBreakStatement();
			case "do":
				next();
				return parseDoStatement();
			case "goto":
				next();
				return parseGotoStatement();
		}
	}

	if (
		features.contextualGoto &&
		token.type === Identifier &&
		token.value === "goto" &&
		lookahead.type === Identifier &&
		lookahead.value !== "goto"
	) {
		next();
		return parseGotoStatement();
	}

	if (Punctuator === token.type) {
		if (consume("::")) return parseLabelStatement();
	}
	// Assignments memorizes the location and pushes it manually for wrapper
	// nodes. Additionally empty `;` statements should not mark a location.
	if (trackLocations) locations.pop();

	// When a `;` is encounted, simply eat it without storing it.
	if (features.emptyStatement) {
		if (consume(";")) return;
	}

	return parseAssignmentOrCallStatement();
}

// ## Statements

//     label ::= '::' Name '::'

function parseLabelStatement() {
	const name: any = token.value,
		label = parseIdentifier();

	scopeIdentifierName("::" + name + "::");
	attachScope(label, true);

	expect("::");
	return finishNode(ast.labelStatement(label));
}

//     break ::= 'break'

function parseBreakStatement() {
	return finishNode(ast.breakStatement());
}

//     goto ::= 'goto' Name

function parseGotoStatement() {
	const label = parseIdentifier();

	return finishNode(ast.gotoStatement(label));
}

//     do ::= 'do' block 'end'

function parseDoStatement() {
	createScope(false);
	const body = parseBlock();
	destroyScope();
	expect("end");
	return finishNode(ast.doStatement(body));
}

//     while ::= 'while' exp 'do' block 'end'

function parseWhileStatement() {
	const condition = parseExpectedExpression();
	expect("do");
	createScope(false);
	const body = parseBlock();
	destroyScope();
	expect("end");
	return finishNode(ast.whileStatement(condition, body));
}

//     repeat ::= 'repeat' block 'until' exp

function parseRepeatStatement() {
	createScope(false);
	const body = parseBlock();
	expect("until");
	const condition = parseExpectedExpression();
	destroyScope();
	return finishNode(ast.repeatStatement(condition, body));
}

//     retstat ::= 'return' [exp {',' exp}] [';']

function parseReturnStatement() {
	const expressions = [];

	if ("end" !== token.value) {
		let expression = parseExpression();
		if (null != expression) expressions.push(expression);
		while (consume(",")) {
			expression = parseExpectedExpression();
			expressions.push(expression);
		}
		consume(";"); // grammar tells us ; is optional here.
	}
	return finishNode(ast.returnStatement(expressions));
}

//     if ::= 'if' exp 'then' block {elif} ['else' block] 'end'
//     elif ::= 'elseif' exp 'then' block

function parseIfStatement() {
	const clauses = [];
	let condition;
	let body;
	let marker;

	// IfClauses begin at the same location as the parent IfStatement.
	// It ends at the start of `end`, `else`, or `elseif`.
	if (trackLocations) {
		marker = locations[locations.length - 1];
		locations.push(marker);
	}
	condition = parseExpectedExpression();
	expect("then");
	createScope(false);
	body = parseBlock();
	destroyScope();
	clauses.push(finishNode(ast.ifClause(condition, body)));

	if (trackLocations) marker = createLocationMarker();
	while (consume("elseif")) {
		pushLocation(marker);
		condition = parseExpectedExpression();
		expect("then");
		createScope(false);
		body = parseBlock();
		destroyScope();
		clauses.push(finishNode(ast.elseifClause(condition, body)));
		if (trackLocations) marker = createLocationMarker();
	}

	if (consume("else")) {
		// Include the `else` in the location of ElseClause.
		if (trackLocations) {
			marker = new Marker(previousToken);
			locations.push(marker);
		}
		createScope(false);
		body = parseBlock();
		destroyScope();
		clauses.push(finishNode(ast.elseClause(body)));
	}

	expect("end");
	return finishNode(ast.ifStatement(clauses));
}

// There are two types of for statements, generic and numeric.
//
//     for ::= Name '=' exp ',' exp [',' exp] 'do' block 'end'
//     for ::= namelist 'in' explist 'do' block 'end'
//     namelist ::= Name {',' Name}
//     explist ::= exp {',' exp}

function parseForStatement() {
	let variable = parseIdentifier(),
		body;

	// The start-identifier is local.

	createScope(false);
	scopeIdentifier(variable);

	// If the first expression is followed by a `=` punctuator, this is a
	// Numeric For Statement.
	if (consume("=")) {
		// Start expression
		const start = parseExpectedExpression();
		expect(",");
		// End expression
		const end = parseExpectedExpression();
		// Optional step expression
		const step = consume(",") ? parseExpectedExpression() : null;

		expect("do");
		body = parseBlock();
		expect("end");
		destroyScope();

		return finishNode(
			ast.forNumericStatement(variable, start, end, step, body)
		);
	}
	// If not, it's a Generic For Statement
	else {
		// The namelist can contain one or more identifiers.
		const variables = [variable];
		while (consume(",")) {
			variable = parseIdentifier();
			// Each variable in the namelist is locally scoped.
			scopeIdentifier(variable);
			variables.push(variable);
		}
		expect("in");
		const iterators = [];

		// One or more expressions in the explist.
		do {
			const expression = parseExpectedExpression();
			iterators.push(expression);
		} while (consume(","));

		expect("do");
		body = parseBlock();
		expect("end");
		destroyScope();

		return finishNode(ast.forGenericStatement(variables, iterators, body));
	}
}

// Local statements can either be variable assignments or function
// definitions. If a function definition is found, it will be delegated to
// `parseFunctionDeclaration()` with the isLocal flag.
//
// This AST structure might change into a local assignment with a function
// child.
//
//     local ::= 'local' 'function' Name funcdecl
//        | 'local' Name {',' Name} ['=' exp {',' exp}]

function parseLocalStatement() {
	let name;

	if (Identifier === token.type) {
		const variables = [];
		const init = [];

		do {
			name = parseIdentifier();

			variables.push(name);
		} while (consume(","));

		const types = parseTypeList(true);

		if (consume("=")) {
			do {
				const expression = parseExpectedExpression();
				init.push(expression);
			} while (consume(","));
		}

		// Declarations doesn't exist before the statement has been evaluated.
		// Therefore assignments can't use their declarator. And the identifiers
		// shouldn't be added to the scope until the statement is complete.
		for (let i = 0, l = variables.length; i < l; ++i) {
			scopeIdentifier(variables[i]);
		}

		return finishNode(ast.localStatement(variables, types, init));
	}
	if (consume("function")) {
		name = parseIdentifier();

		scopeIdentifier(name);
		createScope(true);

		// MemberExpressions are not allowed in local function statements.
		return parseFunctionDeclaration(name, true);
	} else {
		throw raiseUnexpectedToken("<name>", token);
	}
}

function validateVar(node: any) {
	// @TODO we need something not dependent on the exact AST used. see also isCallExpression()
	if (
		["Identifier", "MemberExpression", "IndexExpression"].indexOf(node.type) ===
		-1
	) {
		throw raise(token, errors.invalidVar, token.value);
	}
}

//     assignment ::= varlist '=' explist
//     let ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
//     varlist ::= let {',' let}
//     explist ::= exp {',' exp}
//
//     call ::= callexp
//     callexp ::= prefixexp args | prefixexp ':' Name args

function parseAssignmentOrCallStatement() {
	// Keep a reference to the previous token for better error messages in case
	// of invalid statement
	const previous = token;
	let marker;

	if (trackLocations) marker = createLocationMarker();
	const expression = parsePrefixExpression();

	if (null == expression) throw unexpected(token);
	if (",=".indexOf((token.value: any)) >= 0) {
		const variables = [expression];
		const init = [];
		let exp;

		validateVar(expression);
		while (consume(",")) {
			exp = parsePrefixExpression();
			if (null == exp) throw raiseUnexpectedToken("<expression>", token);
			validateVar(exp);
			variables.push(exp);
		}
		expect("=");
		do {
			exp = parseExpectedExpression();
			init.push(exp);
		} while (consume(","));

		pushLocation(marker);
		return finishNode(ast.assignmentStatement(variables, init));
	}
	if (isCallExpression(expression)) {
		pushLocation(marker);
		return finishNode(ast.callStatement(expression));
	}
	// The prefix expression was neither part of an assignment or a
	// callstatement, however as it was valid it's been consumed, so throw raise
	// the exception on the previous token to provide a helpful message.
	throw unexpected(previous);
}

// ### Non-statements

function parseFuncTypeArgs() {
	expect("(");
	if (!consume(")")) {
		const typeList = parseTypeList(false);
		expect(")");
		return typeList;
	}
	return ast.typeList([], nil_type);
}

//     functype ::= functypeargs '=>' functypeargs
//     functypeargs ::= '(' [typelist] ')'
function parseFuncType() {
	const parameters = parseFuncTypeArgs();
	expect("=>");
	const returns = parseFuncTypeArgs();
	return finishNode(ast.functionType(parameters, returns));
}

//     tabletype ::= '{' {name ':' typeinfo ','} name ':' typeinfo [','] '}'
//     tabletype ::= '{' '}'
function parseTableType() {
	expect("{");
	const map = new Map();
	while (!consume("}")) {
		const name = parseIdentifier();
		expect(":");
		const type = parseTypeInfo();
		map.set(name.name, type);
		if (!consume(",")) {
			expect("}");
			break;
		}
	}
	return finishNode(ast.tableType(map));
}

function parseTypeInfo() {
	const s = new Set();
	s.add(parseSingleType());
	while (consume("|")) {
		s.add(parseSingleType());
	}
	return finishNode(ast.typeInfo(s));
}

//     singletype ::= 'number' | 'boolean' | 'string' | 'table' | 'function' | 'nil' | 'any' | functype
//     singletype ::= functype
//     singletype ::= tabletype
function parseSingleType() {
	let type;
	if (token.type === Punctuator && token.value === "(") return parseFuncType();
	else if (token.type === Punctuator && token.value === "{")
		return parseTableType();
	else if (token.type === Identifier) type = token.value;
	else if (token.type === NilLiteral) type = "nil";
	else if (token.type === Keyword && token.value === "function")
		type = "function";
	else throw raiseUnexpectedToken("<type>", token);
	switch (type) {
		case "number":
		case "boolean":
		case "string":
		case "table":
		case "function":
		case "nil":
		case "any":
			next();
			return finishNode(ast.simpleType(type));
		default:
			throw raiseUnexpectedToken("<type>", token);
	}
}

//     typelist ::= ':' { typeinfo ',' } typeinfo
//     typelist ::=
function parseTypeList(parseColon) {
	if (parseColon && !consume(":"))
		return finishNode(ast.typeList([], any_type));
	const types = [parseTypeInfo()];
	while (consume(",")) types.push(parseTypeInfo());
	return finishNode(ast.typeList(types, nil_type));
}

//     Identifier ::= Name

function parseIdentifier() {
	markLocation();
	const identifier = token.value;
	if (Identifier !== token.type) throw raiseUnexpectedToken("<name>", token);
	next();
	return finishNode(ast.identifier(identifier));
}

// Parse the functions parameters and body block. The name should already
// have been parsed and passed to this declaration function. By separating
// this we allow for anonymous functions in expressions.
//
// For local functions there's a boolean parameter which needs to be set
// when parsing the declaration.
//
//     funcdecl ::= '(' [parlist] ')' block 'end'
//     parlist ::= Name {',' Name} | [',' '...'] | '...'

function parseFunctionDeclaration(name, isLocal) {
	const parameters = [];
	let parameter_types = null;
	let return_types = null;
	expect("(");
	let has_varargs = false;

	// The declaration has arguments
	if (!consume(")")) {
		// Arguments are a comma separated list of identifiers, optionally ending
		// with a vararg.
		while (true) {
			if (Identifier === token.type) {
				const parameter = parseIdentifier();
				// Function parameters are local.
				scopeIdentifier(parameter);

				parameters.push(parameter);

				if (consume(",")) continue;
				else break;
			}
			// No arguments are allowed after a vararg.
			else if (VarargLiteral === token.type) {
				scopeIdentifierName("...");
				has_varargs = true;
				parsePrimaryExpression(true);
				break;
			} else {
				throw raiseUnexpectedToken("<name> or '...'", token);
			}
		}
		parameter_types = parseTypeList(true);

		expect(")");
	}

	if (consume(":")) {
		if (token.type === Identifier && token.value === "void") {
			return_types = finishNode(ast.typeList([], nil_type));
			next();
		} else return_types = parseTypeList(false);
	} else return_types = ast.typeList([], any_type);

	const body = parseBlock();
	expect("end");
	destroyScope();

	if (parameter_types == null) parameter_types = ast.typeList([], nil_type);

	return finishNode(
		ast.functionStatement(
			name,
			parameters,
			parameter_types,
			return_types,
			has_varargs,
			isLocal || false,
			body
		)
	);
}

// Parse the function name as identifiers and member expressions.
//
//     Name {'.' Name} [':' Name]

function parseFunctionName() {
	let base, name, marker;

	if (trackLocations) marker = createLocationMarker();
	base = parseIdentifier();

	attachScope(base, scopeHasName(base.name));
	createScope(true);

	while (consume(".")) {
		pushLocation(marker);
		name = parseIdentifier();
		base = finishNode(ast.memberExpression(base, ".", name));
	}

	if (consume(":")) {
		pushLocation(marker);
		name = parseIdentifier();
		base = finishNode(ast.memberExpression(base, ":", name));
		scopeIdentifierName("self");
	}

	return base;
}

//     tableconstructor ::= '{' [fieldlist] '}'
//     fieldlist ::= field {fieldsep field} fieldsep
//     field ::= '[' exp ']' '=' exp | Name = 'exp' | exp
//
//     fieldsep ::= ',' | ';'

function parseTableConstructor() {
	const fields = [];
	let key;
	let value;

	while (true) {
		markLocation();
		if (Punctuator === token.type && consume("[")) {
			key = parseExpectedExpression();
			expect("]");
			expect("=");
			value = parseExpectedExpression();
			fields.push(finishNode(ast.tableKey(key, value)));
		} else if (Identifier === token.type) {
			if ("=" === lookahead.value) {
				key = parseIdentifier();
				next();
				value = parseExpectedExpression();
				fields.push(finishNode(ast.tableKeyString(key, value)));
			} else {
				value = parseExpectedExpression();
				fields.push(finishNode(ast.tableValue(value)));
			}
		} else {
			if (null == (value = parseExpression())) {
				locations.pop();
				break;
			}
			fields.push(finishNode(ast.tableValue(value)));
		}
		if (",;".indexOf((token.value: any)) >= 0) {
			next();
			continue;
		}
		break;
	}
	expect("}");
	return finishNode(ast.tableConstructorExpression(fields));
}

// Expression parser
// -----------------
//
// Expressions are evaluated and always return a value. If nothing is
// matched null will be returned.
//
//     exp ::= (unop exp | primary | prefixexp ) { binop exp }
//
//     primary ::= nil | false | true | Number | String | '...'
//          | functiondef | tableconstructor
//
//     prefixexp ::= (Name | '(' exp ')' ) { '[' exp ']'
//          | '.' Name | ':' Name args | args }
//

function parseExpression() {
	const expression = parseSubExpression(0);
	return expression;
}

// Parse an expression expecting it to be valid.

function parseExpectedExpression() {
	const expression = parseExpression();
	if (null == expression) throw raiseUnexpectedToken("<expression>", token);
	else return expression;
}

// Return the precedence priority of the operator.
//
// As unary `-` can't be distinguished from binary `-`, unary precedence
// isn't described in this table but in `parseSubExpression()` itself.
//
// As this function gets hit on every expression it's been optimized due to
// the expensive CompareICStub which took ~8% of the parse time.

function binaryPrecedence(operator: any) {
	const charCode = operator.charCodeAt(0),
		length = operator.length;

	if (1 === length) {
		switch (charCode) {
			case 94:
				return 12; // ^
			case 42:
			case 47:
			case 37:
				return 10; // * / %
			case 43:
			case 45:
				return 9; // + -
			case 38:
				return 6; // &
			case 126:
				return 5; // ~
			case 124:
				return 4; // |
			case 60:
			case 62:
				return 3; // < >
		}
	} else if (2 === length) {
		switch (charCode) {
			case 47:
				return 10; // //
			case 46:
				return 8; // ..
			case 60:
			case 62:
				if ("<<" === operator || ">>" === operator) return 7; // << >>
				return 3; // <= >=
			case 61:
			case 126:
				return 3; // == ~=
			case 111:
				return 1; // or
		}
	} else if (97 === charCode && "and" === operator) return 2;
	return 0;
}

// Implement an operator-precedence parser to handle binary operator
// precedence.
//
// We use this algorithm because it's compact, it's fast and Lua core uses
// the same so we can be sure our expressions are parsed in the same manner
// without excessive amounts of tests.
//
//     exp ::= (unop exp | primary | prefixexp ) { binop exp }

function parseSubExpression(minPrecedence) {
	let operator = token.value,
		// The left-hand side in binary operations.
		expression,
		marker;

	if (trackLocations) marker = createLocationMarker();

	// UnaryExpression
	if (isUnary(token)) {
		markLocation();
		next();
		const argument = parseSubExpression(10);
		if (argument == null) throw raiseUnexpectedToken("<expression>", token);
		expression = finishNode(ast.unaryExpression(operator, argument));
	}
	if (null == expression) {
		// PrimaryExpression
		expression = parsePrimaryExpression();

		// PrefixExpression
		if (null == expression) {
			expression = parsePrefixExpression();
		}
	}
	// This is not a valid left hand expression.
	if (null == expression) return null;

	let precedence;
	while (true) {
		operator = token.value;

		precedence =
			Punctuator === token.type || Keyword === token.type
				? binaryPrecedence(operator)
				: 0;

		if (precedence === 0 || precedence <= minPrecedence) break;
		// Right-hand precedence operators
		if ("^" === operator || ".." === operator) precedence--;
		next();
		const right = parseSubExpression(precedence);
		if (null == right) throw raiseUnexpectedToken("<expression>", token);
		// Push in the marker created before the loop to wrap its entirety.
		if (trackLocations) locations.push(marker);
		expression = finishNode(ast.binaryExpression(operator, expression, right));
	}
	return expression;
}

//     prefixexp ::= prefix {suffix}
//     prefix ::= Name | '(' exp ')'
//     suffix ::= '[' exp ']' | '.' Name | ':' Name args | args
//
//     args ::= '(' [explist] ')' | tableconstructor | String

function parsePrefixExpression() {
	let base, name, marker;

	if (trackLocations) marker = createLocationMarker();

	// The prefix
	if (Identifier === token.type) {
		name = token.value;
		base = parseIdentifier();
		// Set the parent scope.
		attachScope(base, scopeHasName(name));
	} else if (consume("(")) {
		base = parseExpectedExpression();
		expect(")");
		base = finishNode(ast.parenthesisExpression(base));
	} else {
		return null;
	}

	// The suffix
	let expression, identifier;
	while (true) {
		if (Punctuator === token.type) {
			switch (token.value) {
				case "[":
					pushLocation(marker);
					next();
					expression = parseExpectedExpression();
					expect("]");
					base = finishNode(ast.indexExpression(base, expression));
					break;
				case ".":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					base = finishNode(ast.memberExpression(base, ".", identifier));
					break;
				case ":":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					base = finishNode(ast.memberExpression(base, ":", identifier));
					// Once a : is found, this has to be a CallExpression, otherwise
					// throw an error.
					pushLocation(marker);
					base = parseCallExpression(base);
					break;
				case "(":
				case "{": // args
					pushLocation(marker);
					base = parseCallExpression(base);
					break;
				default:
					return base;
			}
		} else if (StringLiteral === token.type) {
			pushLocation(marker);
			base = parseCallExpression(base);
		} else {
			break;
		}
	}

	return base;
}

//     args ::= '(' [explist] ')' | tableconstructor | String

function parseCallExpression(base) {
	if (Punctuator === token.type) {
		switch (token.value) {
			case "(": {
				if (!features.emptyStatement) {
					if (token.line !== previousToken.line)
						throw raise({}, errors.ambiguousSyntax, token.value);
				}
				next();

				// List of expressions
				const expressions = [];
				let expression = parseExpression();
				if (null != expression) expressions.push(expression);
				while (consume(",")) {
					expression = parseExpectedExpression();
					expressions.push(expression);
				}

				expect(")");
				return finishNode(ast.callExpression(base, expressions));
			}
			case "{": {
				markLocation();
				next();
				const table = parseTableConstructor();
				return finishNode(ast.tableCallExpression(base, table));
			}
		}
	} else if (StringLiteral === token.type) {
		return finishNode(ast.stringCallExpression(base, parsePrimaryExpression()));
	}

	throw raiseUnexpectedToken("function arguments", token);
}

//     primary ::= String | Numeric | nil | true | false
//          | functiondef | tableconstructor | '...'
// if identifier is true, it is parsing an identifier so does not
// check for error with '...'
function parsePrimaryExpression(identifier) {
	const literals =
		StringLiteral |
		NumericLiteral |
		BooleanLiteral |
		NilLiteral |
		VarargLiteral;
	const value = token.value;
	const type = token.type;
	let marker;

	if (trackLocations) marker = createLocationMarker();

	if (type & literals) {
		if (!identifier && type === VarargLiteral && !scopeHasName("..."))
			throw raise(token, "cannot use '...' outside a vararg function");
		pushLocation(marker);
		const raw = input.slice(token.range[0], token.range[1]);
		next();
		return finishNode(ast.literal(type, value, raw));
	} else if (Keyword === type && "function" === value) {
		pushLocation(marker);
		next();
		createScope(true);
		return parseFunctionDeclaration(null);
	} else if (consume("{")) {
		pushLocation(marker);
		return parseTableConstructor();
	}
}

// Parser
// ------

// Export the main parser.
//
//   - `wait` Hold parsing until end() is called. Defaults to false
//   - `comments` Store comments. Defaults to true.
//   - `locations` Store location information. Defaults to false.
//   - `ranges` Store the start and end character locations. Defaults to
//     false.
//
// Example:
//
//     let parser = require('luaparser');
//     parser.parse('i = 0');

let gen;
let input;

function lex() {
	const x = gen.next();
	if (x.done) return { type: 1 };
	else return x.value;
}

// These are only the features useful for parsing
const versionFeatures = {
	"5.1": {},
	"5.2": {
		emptyStatement: true,
	},
	"5.3": {
		emptyStatement: true,
	},
	LuaJIT: {
		// XXX: LuaJIT language features may depend on compilation options; may need to
		// rethink how to handle this. Specifically, there is a LUAJIT_ENABLE_LUA52COMPAT
		// that removes contextual goto. Maybe add 'LuaJIT-5.2compat' as well?
		contextualGoto: true,
	},
};

function parse(_input: any, _options: any): any {
	// $FlowFixMe
	if (!_options) _options = {}; // eslint-disable-line no-param-reassign

	options = extend(defaultOptions, _options);

	// When tracking identifier scope, initialize with an empty scope.
	scopes = [];
	function_scope = [];
	scopeDepth = -1;
	globals = [];
	locations = [];
	input = _input;

	if (!(features = versionFeatures[options.luaVersion])) {
		throw new Error(`Lua version '${options.luaVersion}' not supported`);
	}

	if (options.comments) comments = [];

	trackLocations = options.locations || options.ranges;
	gen = tokenize(_input, _options);
	// Initialize with a lookahead token.
	lookahead = lex();

	const chunk = parseChunk();
	if (options.comments) chunk.comments = comments;
	chunk.globals = globals;

	if (locations.length > 0)
		throw new Error(
			"Location tracking failed. This is most likely a bug in luaparse"
		);

	return chunk;
}

const parse_: (string, LuaParseOptions) => AST.Chunk = parse;

export { ast, parse_ as parse };
