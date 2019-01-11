// @flow

import * as AST from "./ast-types";
import * as Token from "./token-types";
import { astError, errors, tokenError } from "./errors";
import fs from "fs";
import invariant from "assert";
import { tokenize } from "./lua-tokenize";

const Placeholder = 1;
const StringLiteral = 2;
const Keyword = 4;
const Identifier = 8;
const NumericLiteral = 16;
const Punctuator = 32;
const BooleanLiteral = 64;
const NilLiteral = 128;
const VarargLiteral = 256;

// ### Abstract Syntax Tree
//
// The default AST structure is inspired by the Mozilla Parser API but can
// easily be customized by overriding these functions.

export const ast = {
	labelStatement(label: AST.Identifier): AST.LabelStatement {
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

	gotoStatement(label: AST.Identifier): AST.GotoStatement {
		return {
			type: "GotoStatement",
			label,
		};
	},

	returnStatement(args: Array<AST.Expression>): AST.ReturnStatement {
		return {
			type: "ReturnStatement",
			args,
		};
	},

	ifStatement(
		clauses: Array<AST.IfClause | AST.ElseifClause | AST.ElseClause>
	): AST.IfStatement {
		return {
			type: "IfStatement",
			clauses,
		};
	},

	ifClause(condition: AST.Expression, body: AST.SimpleBlock): AST.IfClause {
		return {
			type: "IfClause",
			condition,
			body,
		};
	},

	elseifClause(
		condition: AST.Expression,
		body: AST.SimpleBlock
	): AST.ElseifClause {
		return {
			type: "ElseifClause",
			condition,
			body,
		};
	},

	elseClause(body: AST.SimpleBlock): AST.ElseClause {
		return {
			type: "ElseClause",
			body,
		};
	},

	whileStatement(
		condition: AST.Expression,
		body: AST.SimpleBlock
	): AST.WhileStatement {
		return {
			type: "WhileStatement",
			condition,
			body,
		};
	},

	doStatement(body: AST.SimpleBlock): AST.DoStatement {
		return {
			type: "DoStatement",
			body,
		};
	},

	repeatStatement(
		condition: AST.Expression,
		body: AST.SimpleBlock
	): AST.RepeatStatement {
		return {
			type: "RepeatStatement",
			condition,
			body,
		};
	},

	localStatement(
		kind: "local" | "const",
		variables: Array<AST.Identifier>,
		typeList: AST.TypeList,
		init: Array<AST.Expression>
	): AST.LocalStatement {
		return {
			type: "LocalStatement",
			kind,
			variables,
			typeList,
			init,
		};
	},

	assignmentStatement(
		variables: Array<AST.Variable>,
		init: Array<AST.Expression>
	): AST.AssignmentStatement {
		return {
			type: "AssignmentStatement",
			variables,
			init,
		};
	},

	callStatement(
		expression:
			| AST.CallExpression
			| AST.StringCallExpression
			| AST.TableCallExpression
	): AST.CallStatement {
		return {
			type: "CallStatement",
			expression,
		};
	},

	functionBase(
		parameters: Array<AST.Identifier>,
		has_varargs: boolean,
		parameter_types: AST.TypeList,
		body: AST.FunctionBlock
	): AST.FunctionBase {
		return {
			parameters,
			has_varargs,
			parameter_types,
			body,
		};
	},

	localFunctionStatement(
		identifier: AST.Identifier,
		kind: "local" | "const",
		base: AST.FunctionBase
	): AST.LocalFunctionStatement {
		return {
			type: "LocalFunctionStatement",
			kind,
			identifier,
			...base,
		};
	},

	nonLocalFunctionStatement(
		identifier: AST.NonLocalFunctionName,
		base: AST.FunctionBase
	): AST.NonLocalFunctionStatement {
		return {
			type: "NonLocalFunctionStatement",
			identifier,
			...base,
		};
	},

	functionExpression(base: AST.FunctionBase): AST.FunctionExpression {
		return {
			type: "FunctionExpression",
			...base,
		};
	},

	forNumericStatement(
		variable: AST.Identifier,
		start: AST.Expression,
		end: AST.Expression,
		step: ?AST.Expression,
		body: AST.SimpleBlock
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
		variables: Array<AST.Identifier>,
		iterators: Array<AST.Expression>,
		body: AST.SimpleBlock
	): AST.ForGenericStatement {
		return {
			type: "ForGenericStatement",
			variables,
			iterators,
			body,
		};
	},

	chunk(body: AST.FunctionBlock, meta: AST.MetaInfo): AST.Chunk {
		return {
			type: "Chunk",
			body,
			meta,
		};
	},

	identifier(name: string): AST.Identifier {
		return {
			type: "Identifier",
			name,
		};
	},

	functionType(
		parameters: AST.TypeList,
		returns: AST.TypeList
	): AST.FunctionType {
		return {
			type: "FunctionType",
			parameter_types: parameters,
			return_types: returns,
		};
	},

	tableType(typeMap: Map<string, AST.TypeInfo>): AST.TableType {
		return {
			type: "TableType",
			typeMap,
		};
	},

	simpleType(value: $PropertyType<AST.SimpleType, "value">): AST.SimpleType {
		return {
			type: "SimpleType",
			value,
		};
	},

	typeInfo(possibleTypes: Set<AST.SingleType>): AST.TypeInfo {
		return {
			type: "TypeInfo",
			possibleTypes,
		};
	},

	typeList(list: Array<AST.TypeInfo>, rest: AST.TypeInfo): AST.TypeList {
		return {
			type: "TypeList",
			list,
			rest,
		};
	},

	// to type this better we should split it
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

	parenthesisExpression(expression: AST.Expression): AST.ParenthesisExpression {
		return {
			type: "ParenthesisExpression",
			expression,
		};
	},

	tableKey(key: AST.Expression, value: AST.Expression): AST.TableKey {
		return {
			type: "TableKey",
			key,
			value,
		};
	},

	tableKeyString(
		key: AST.Identifier,
		value: AST.Expression
	): AST.TableKeyString {
		return {
			type: "TableKeyString",
			key,
			value,
		};
	},

	tableValue(value: AST.Expression): AST.TableValue {
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
		base: AST.Expression,
		indexer: "." | ":",
		identifier: AST.Identifier
	): AST.MemberExpression {
		if (indexer === ".")
			return {
				type: "MemberExpression",
				indexer: ".",
				identifier,
				base,
			};
		else
			return {
				type: "MemberExpression",
				indexer: ":",
				identifier,
				base,
			};
	},

	indexExpression(
		base: AST.Expression,
		index: AST.Expression
	): AST.IndexExpression {
		return {
			type: "IndexExpression",
			base,
			index,
		};
	},

	callExpression(
		base: AST.Expression | AST.ColonMemberExpression,
		args: Array<AST.Expression>
	): AST.CallExpression {
		return {
			type: "CallExpression",
			base,
			args,
		};
	},

	tableCallExpression(
		base: AST.Expression | AST.ColonMemberExpression,
		args: AST.TableConstructorExpression
	): AST.TableCallExpression {
		return {
			type: "TableCallExpression",
			base,
			args: [args],
		};
	},

	stringCallExpression(
		base: AST.Expression | AST.ColonMemberExpression,
		argument: AST.StringLiteral
	): AST.StringCallExpression {
		return {
			type: "StringCallExpression",
			base,
			args: [argument],
		};
	},

	simpleBlock(statements: $ReadOnlyArray<AST.Statement>): AST.SimpleBlock {
		return {
			type: "SimpleBlock",
			statements,
		};
	},

	functionBlock(
		statements: $ReadOnlyArray<AST.Statement>,
		return_types: AST.TypeList
	): AST.FunctionBlock {
		return {
			type: "FunctionBlock",
			statements,
			return_types,
		};
	},

	declareStatement(
		identifier: AST.Identifier,
		typeInfo: AST.TypeInfo
	): AST.DeclareStatement {
		return {
			type: "DeclareStatement",
			identifier,
			typeInfo,
		};
	},

	comment(value: string, raw: string) {
		return {
			type: "Comment",
			value,
			raw,
		};
	},
};

const nil_type = ast.typeInfo(new Set([ast.simpleType("nil")]));
const any_type = ast.typeInfo(new Set([ast.simpleType("any")]));
const empty_type = ast.typeInfo(new Set([ast.simpleType("empty")]));

// Parser
// ------

// Export the main parser.
//
//	 - `wait` Hold parsing until end() is called. Defaults to false
//	 - `comments` Store comments. Defaults to true.
//	 - `locations` Store location information. Defaults to false.
//	 - `ranges` Store the start and end character locations. Defaults to
//	   false.
//
// Example:
//
//	   let parser = require('luaparser');
//	   parser.parse('i = 0');

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
	+onlyReturnType?: boolean,
	+features?: {|
		+const_?: boolean,
		+typeCheck?: boolean,
	|},
|};

// Options can be set either globally on the parser objec:t through
// defaultOptions, or during the parse call.
const defaultOptions: LuaParseOptions = {
	comments: true,
	locations: true,
	ranges: true,
	luaVersion: "5.1",
	extendedIdentifiers: false,
	onlyReturnType: false,
	features: {
		const_: false,
		typeCheck: false,
	},
};

export async function parseFile(
	file: string,
	options?: LuaParseOptions
): Promise<AST.Chunk> {
	const code: string = await new Promise((resolve, reject) => {
		fs.readFile(file, (err, data) =>
			err ? reject(err) : resolve(data.toString())
		);
	});
	return parse(code, { code, filename: file }, options);
}

export function parse(
	input: string,
	meta_: ?AST.MetaInfo,
	_options?: LuaParseOptions
): AST.Chunk {
	const meta = { ...meta_, code: input };
	const options = { ...defaultOptions, ..._options };
	const features = {
		...versionFeatures[options.luaVersion],
		...options.features,
	};

	// When tracking identifier scope, initialize with an empty scope.
	const scopes = [];
	const function_scope = [];
	let scopeDepth = -1;
	const locations: Array<Marker> = [];

	let token: Token.Any | Token.Placeholder = {
		type: Placeholder,
		value: "start",
		line: 1,
		lineStart: 1,
		range: [0, 0],
	};
	let previousToken: Token.Any | Token.Placeholder = token;
	let lookahead: Token.Any | Token.Placeholder = token;

	const comments = [];

	const trackLocations = options.locations || options.ranges;

	const gen = tokenize(input, meta, {
		comments: options.comments ? comments : undefined,
		extendedIdentifiers: options.extendedIdentifiers,
		luaVersion: options.luaVersion,
		features: options.features,
	});
	// Initialize with a lookahead token.
	lookahead = lex();

	class Marker {
		loc: $PropertyType<AST.LocationInfo, "loc">;
		range: $PropertyType<AST.LocationInfo, "range">;
		constructor(token: Token.Any | Token.Placeholder) {
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
		complete() {
			if (options.locations) {
				invariant(this.loc != null);
				// $FlowFixMe
				this.loc.end.line = previousToken.lastLine || previousToken.line;
				this.loc.end.column =
					previousToken.range[1] -
					// $FlowFixMe
					(previousToken.lastLineStart || previousToken.lineStart);
			}
			if (options.ranges) {
				invariant(this.range != null);
				this.range[1] = previousToken.range[1];
			}
		}

		bless(node: any) {
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
		}
	}

	// Below are the functions used by this closure

	function lex(): Token.Any | Token.Placeholder {
		const x = gen.next();
		if (x.done)
			return {
				type: Placeholder,
				value: "EOF",
				line: -1,
				lineStart: -1,
				range: [-1, -1],
			};
		else return x.value;
	}

	// Parse functions
	// ---------------

	// Chunk is the main program object. Syntactically it's the same as a block.
	//
	//	   chunk ::= block

	function parseChunk(): AST.Chunk {
		next();
		markLocation();
		createScope(true);
		scopeIdentifierName("...");
		const body = parseFunctionBlock();
		destroyScope();
		if (
			!options.onlyReturnType &&
			(Placeholder !== token.type || token.value !== "EOF")
		)
			throw tokenError(errors.expectedType, meta, token, "<eof>");
		// If the body is empty no previousToken exists when finishNode runs.
		if (trackLocations && !body.statements.length) previousToken = token;
		return finishNode(ast.chunk(body, meta));
	}

	// A block contains a list of statements with an optional return statement
	// as its last statement.
	//
	//	   block ::= {stat} [retstat]

	function parseBlock(): $ReadOnlyArray<AST.Statement> {
		const block: Array<AST.Statement> = [];
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

	function parseSimpleBlock(): AST.SimpleBlock {
		return ast.simpleBlock(parseBlock());
	}

	function parseFunctionBlock(): AST.FunctionBlock {
		let return_types;
		if (features.typeCheck && consume(":")) {
			if (token.type === Identifier && token.value === "void") {
				return_types = ast.typeList([], empty_type);
				next();
			} else return_types = parseTypeList(false);
		} else return_types = ast.typeList([], any_type);
		if (options.onlyReturnType) return ast.functionBlock([], return_types);
		return ast.functionBlock(parseBlock(), return_types);
	}

	// There are two types of statements, simple and compound.
	//
	//	   statement ::= break | goto | do | while | repeat | return
	//			| if | for | function | local | label | assignment
	//			| functioncall | ';'

	function parseStatement(): AST.Statement {
		markLocation();
		if (Keyword === token.type) {
			switch (token.value) {
				case "declare":
					invariant(features.typeCheck);
					next();
					return parseDeclareStatement();
				case "local":
				case "const":
					next();
					return parseLocalStatement((previousToken.value: any));
				case "if":
					next();
					return parseIfStatement();
				case "return":
					next();
					return parseReturnStatement();
				case "function": {
					next();
					const name = parseFunctionName();
					const base = parseFunctionBase();
					return finishNode(ast.nonLocalFunctionStatement(name, base));
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

		// TODO: don't do this
		// When a `;` is encounted, simply eat it without storing it.
		if (features.emptyStatement) {
			// $FlowFixMe
			if (consume(";")) return;
		}

		return parseAssignmentOrCallStatement();
	}

	// ## Statements

	//	   label ::= '::' Name '::'

	function parseLabelStatement(): AST.LabelStatement {
		invariant(token.type === Identifier);
		const name = token.value;
		const label = parseIdentifier();

		scopeIdentifierName("::" + name + "::");

		expect("::");
		return finishNode(ast.labelStatement(label));
	}

	//	   break ::= 'break'

	function parseBreakStatement(): AST.BreakStatement {
		return finishNode(ast.breakStatement());
	}

	//	   goto ::= 'goto' Name

	function parseGotoStatement(): AST.GotoStatement {
		const label = parseIdentifier();

		return finishNode(ast.gotoStatement(label));
	}

	//	   do ::= 'do' block 'end'

	function parseDoStatement(): AST.DoStatement {
		createScope(false);
		const body = parseSimpleBlock();
		destroyScope();
		expect("end");
		return finishNode(ast.doStatement(body));
	}

	//	   while ::= 'while' exp 'do' block 'end'

	function parseWhileStatement(): AST.WhileStatement {
		const condition = parseExpectedExpression();
		expect("do");
		createScope(false);
		const body = parseSimpleBlock();
		destroyScope();
		expect("end");
		return finishNode(ast.whileStatement(condition, body));
	}

	//	   repeat ::= 'repeat' block 'until' exp

	function parseRepeatStatement(): AST.RepeatStatement {
		createScope(false);
		const body = parseSimpleBlock();
		expect("until");
		const condition = parseExpectedExpression();
		destroyScope();
		return finishNode(ast.repeatStatement(condition, body));
	}

	//	   retstat ::= 'return' [exp {',' exp}] [';']

	function parseReturnStatement(): AST.ReturnStatement {
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

	//	   if ::= 'if' exp 'then' block {elif} ['else' block] 'end'
	//	   elif ::= 'elseif' exp 'then' block

	function parseIfStatement(): AST.IfStatement {
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
		body = parseSimpleBlock();
		destroyScope();
		clauses.push(finishNode(ast.ifClause(condition, body)));

		if (trackLocations) marker = createLocationMarker();
		while (consume("elseif")) {
			pushLocation(marker);
			condition = parseExpectedExpression();
			expect("then");
			createScope(false);
			body = parseSimpleBlock();
			destroyScope();
			clauses.push(finishNode(ast.elseifClause(condition, body)));
			if (trackLocations) marker = createLocationMarker();
		}

		if (consume("else")) {
			// Include the `else` in the location of ElseClause.
			if (trackLocations) {
				invariant(previousToken.type !== Placeholder);
				marker = new Marker(previousToken);
				locations.push(marker);
			}
			createScope(false);
			body = parseSimpleBlock();
			destroyScope();
			clauses.push(finishNode(ast.elseClause(body)));
		}

		expect("end");
		return finishNode(ast.ifStatement(clauses));
	}

	// There are two types of for statements, generic and numeric.
	//
	//	   for ::= Name '=' exp ',' exp [',' exp] 'do' block 'end'
	//	   for ::= namelist 'in' explist 'do' block 'end'
	//	   namelist ::= Name {',' Name}
	//	   explist ::= exp {',' exp}

	function parseForStatement():
		| AST.ForNumericStatement
		| AST.ForGenericStatement {
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
			body = parseSimpleBlock();
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
			body = parseSimpleBlock();
			expect("end");
			destroyScope();

			return finishNode(ast.forGenericStatement(variables, iterators, body));
		}
	}

	function parseDeclareStatement(): AST.DeclareStatement {
		invariant(features.typeCheck);
		const id = parseIdentifier();
		expect(":");
		const info = parseTypeInfo();
		return finishNode(ast.declareStatement(id, info));
	}

	// Local statements can either be variable assignments or function
	// definitions. If a function definition is found, it will be delegated to
	// `parseFunctionDeclaration()` with the isLocal flag.
	//
	// This AST structure might change into a local assignment with a function
	// child.
	//
	//	   local ::= 'local' 'function' Name funcdecl
	//		  | 'local' Name {',' Name} ['=' exp {',' exp}]

	function parseLocalStatement(
		kind: "local" | "const"
	): AST.LocalStatement | AST.LocalFunctionStatement {
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

			return finishNode(ast.localStatement(kind, variables, types, init));
		}
		expect("function", "name");
		name = parseIdentifier();

		scopeIdentifier(name);
		createScope(true);

		const base = parseFunctionBase();
		return finishNode(ast.localFunctionStatement(name, kind, base));
	}

	function validateVar(node: AST.Expression | AST.ColonMemberExpression) {
		if (
			node.type !== "Identifier" &&
			(node.type !== "MemberExpression" || node.indexer === ":") &&
			node.type !== "IndexExpression"
		) {
			throw astError(errors.invalidVar, meta, node);
		}
	}

	//	   assignment ::= varlist '=' explist
	//	   let ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
	//	   varlist ::= let {',' let}
	//	   explist ::= exp {',' exp}
	//
	//	   call ::= callexp
	//	   callexp ::= prefixexp args | prefixexp ':' Name args

	function parseAssignmentOrCallStatement():
		| AST.AssignmentStatement
		| AST.CallStatement {
		let marker;

		if (trackLocations) marker = createLocationMarker();
		const expression = parsePrefixExpression();

		if (null == expression)
			throw tokenError(errors.unexpectedToken, meta, token);
		if (token.type === Punctuator && ",=".indexOf(token.value) >= 0) {
			// $FlowFixMe
			const variables: Array<AST.Variable> = [expression];
			const init = [];
			let exp;

			validateVar(expression);
			while (consume(",")) {
				exp = parsePrefixExpression();
				if (null == exp)
					throw tokenError(errors.expectedType, meta, token, "<expression>");
				validateVar(exp);
				invariant(
					exp.type === "Identifier" ||
						exp.type === "IndexExpression" ||
						(exp.type === "MemberExpression" && exp.indexer === ".")
				);
				variables.push(exp);
			}
			expect("=");
			do {
				exp = parseExpectedExpression();
				init.push(exp);
			} while (consume(","));

			pushLocation(marker);
			return finishNode(ast.assignmentStatement(variables, init));
		} else if (isCallExpression(expression)) {
			invariant(
				expression.type === "CallExpression" ||
					expression.type === "StringCallExpression" ||
					expression.type === "TableCallExpression"
			);
			pushLocation(marker);
			return finishNode(ast.callStatement(expression));
		}
		// The prefix expression was neither part of an assignment or a
		// callstatement, however as it was valid it's been consumed, so throw raise
		// the exception on the previous token to provide a helpful message.
		throw tokenError(errors.unexpectedToken, meta, token);
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

	//	   functype ::= functypeargs '=>' functypeargs
	//	   functypeargs ::= '(' [typelist] ')'
	function parseFuncType(): AST.FunctionType {
		const parameters = parseFuncTypeArgs();
		expect("=>");
		const returns = parseFuncTypeArgs();
		return ast.functionType(parameters, returns);
	}

	//	   tabletype ::= '{' {name ':' typeinfo ','} name ':' typeinfo [','] '}'
	//	   tabletype ::= '{' '}'
	function parseTableType(): AST.TableType {
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
		return ast.tableType(map);
	}

	function parseTypeInfo(): AST.TypeInfo {
		const s = new Set();
		s.add(parseSingleType());
		while (consume("|")) {
			s.add(parseSingleType());
		}
		return ast.typeInfo(s);
	}

	//	   singletype ::= 'number' | 'boolean' | 'string' | 'table' | 'function' | 'nil' | 'any' | functype
	//	   singletype ::= functype
	//	   singletype ::= tabletype
	function parseSingleType(): AST.SingleType {
		let type;
		if (token.type === Punctuator && token.value === "(")
			return parseFuncType();
		else if (token.type === Punctuator && token.value === "{")
			return parseTableType();
		else if (token.type === Identifier) type = token.value;
		else if (token.type === NilLiteral) type = "nil";
		else if (token.type === Keyword && token.value === "function")
			type = "function";
		else throw tokenError(errors.expectedType, meta, token, "<type>");
		switch (type) {
			case "number":
			case "boolean":
			case "string":
			case "table":
			case "function":
			case "nil":
			case "any":
				//case "empty": Can't explicitly say empty
				next();
				return ast.simpleType(type);
			default:
				throw tokenError(errors.expectedType, meta, token, "<type>");
		}
	}

	//	   typelist ::= ':' { typeinfo ',' } typeinfo
	//	   typelist ::=
	function parseTypeList(parseColon): AST.TypeList {
		if (!parseColon) invariant(features.typeCheck);
		if (parseColon && (!features.typeCheck || !consume(":")))
			return ast.typeList([], any_type);
		const types = [];
		do {
			if (consume("...")) {
				const rest = parseTypeInfo();
				rest.possibleTypes.add(ast.simpleType("nil"));
				return ast.typeList(types, rest);
			}
			types.push(parseTypeInfo());
		} while (consume(","));
		return ast.typeList(types, empty_type);
	}

	//	   Identifier ::= Name

	function parseIdentifier(): AST.Identifier {
		if (Identifier !== token.type)
			throw tokenError(errors.expectedType, meta, token, "<name>");
		markLocation();
		const identifier = token.value;
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
	//	   funcdecl ::= '(' [parlist] ')' block 'end'
	//	   parlist ::= Name {',' Name} | [',' '...'] | '...'

	function parseFunctionBase(): AST.FunctionBase {
		const parameters = [];
		let parameter_types = null;
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
					throw tokenError(errors.expectedType, meta, token, "<name> or «...»");
				}
			}
			parameter_types = parseTypeList(true);

			expect(")");
		}

		const body = parseFunctionBlock();
		expect("end");
		destroyScope();

		if (parameter_types == null) parameter_types = ast.typeList([], empty_type);

		return ast.functionBase(parameters, has_varargs, parameter_types, body);
	}

	// Parse the function name as identifiers and member expressions.
	//
	//	   Name {'.' Name} [':' Name]

	function parseFunctionName(): AST.NonLocalFunctionName {
		let name, marker;

		if (trackLocations) marker = createLocationMarker();
		let base = parseIdentifier();

		createScope(true);

		while (consume(".")) {
			pushLocation(marker);
			name = parseIdentifier();
			// $FlowFixMe Damn ColonMemberExpression
			base = finishNode(ast.memberExpression(base, ".", name));
		}

		if (consume(":")) {
			pushLocation(marker);
			name = parseIdentifier();
			// $FlowFixMe Damn ColonMemberExpression
			base = finishNode(ast.memberExpression(base, ":", name));
			scopeIdentifierName("self");
		}

		// $FlowFixMe will need to change some stuff
		return base;
	}

	//	   tableconstructor ::= '{' [fieldlist] '}'
	//	   fieldlist ::= field {fieldsep field} fieldsep
	//	   field ::= '[' exp ']' '=' exp | Name = 'exp' | exp
	//
	//	   fieldsep ::= ',' | ';'

	function parseTableConstructor(): AST.TableConstructorExpression {
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
				value = parseExpression();
				if (value == null) {
					locations.pop();
					break;
				}
				fields.push(finishNode(ast.tableValue(value)));
			}
			if (token.type === Punctuator && ",;".indexOf(token.value) >= 0) {
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
	//	   exp ::= (unop exp | primary | prefixexp ) { binop exp }
	//
	//	   primary ::= nil | false | true | Number | String | '...'
	//			| functiondef | tableconstructor
	//
	//	   prefixexp ::= (Name | '(' exp ')' ) { '[' exp ']'
	//			| '.' Name | ':' Name args | args }
	//

	function parseExpression(): ?AST.Expression {
		const expression = parseSubExpression(0);
		return expression;
	}

	// Parse an expression expecting it to be valid.

	function parseExpectedExpression(): AST.Expression {
		const expression = parseExpression();
		if (null == expression)
			throw tokenError(errors.expectedType, meta, token, "<expression>");
		else return expression;
	}

	// Return the precedence priority of the operator.
	//
	// As unary `-` can't be distinguished from binary `-`, unary precedence
	// isn't described in this table but in `parseSubExpression()` itself.
	//
	// As this function gets hit on every expression it's been optimized due to
	// the expensive CompareICStub which took ~8% of the parse time.

	function binaryPrecedence(operator: string): number {
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
	//	   exp ::= (unop exp | primary | prefixexp ) { binop exp }

	function parseSubExpression(minPrecedence): ?AST.Expression {
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
			if (argument == null)
				throw tokenError(errors.expectedType, meta, token, "<expression>");
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
					? binaryPrecedence(token.value)
					: 0;

			if (precedence === 0 || precedence <= minPrecedence) break;
			// Right-hand precedence operators
			if ("^" === operator || ".." === operator) precedence--;
			next();
			const right = parseSubExpression(precedence);
			if (null == right)
				throw tokenError(errors.expectedType, meta, token, "<expression>");
			// Push in the marker created before the loop to wrap its entirety.
			pushLocation(marker);
			expression = finishNode(
				ast.binaryExpression(operator, expression, right)
			);
		}
		invariant(
			expression == null ||
				expression.type !== "MemberExpression" ||
				expression.indexer === "."
		);
		return expression;
	}

	//	   prefixexp ::= prefix {suffix}
	//	   prefix ::= Name | '(' exp ')'
	//	   suffix ::= '[' exp ']' | '.' Name | ':' Name args | args
	//
	//	   args ::= '(' [explist] ')' | tableconstructor | String

	function parsePrefixExpression(): ?(
		| AST.Identifier
		| AST.IndexExpression
		| AST.MemberExpression
		| AST.CallExpression
		| AST.StringCallExpression
		| AST.TableCallExpression
		| AST.ParenthesisExpression
	) {
		let base:
			| AST.Identifier
			| AST.IndexExpression
			| AST.MemberExpression
			| AST.CallExpression
			| AST.StringCallExpression
			| AST.ParenthesisExpression
			| AST.TableCallExpression;
		let marker;

		if (trackLocations) marker = createLocationMarker();

		// The prefix
		if (Identifier === token.type) {
			base = parseIdentifier();
			// Set the parent scope.
		} else if (consume("(")) {
			const inside = parseExpectedExpression();
			expect(")");
			base = ast.parenthesisExpression(inside);
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
						// $FlowFixMe ColonMemberExpression is fucking up everything
						base = finishNode(ast.indexExpression(base, expression));
						break;
					case ".":
						pushLocation(marker);
						next();
						identifier = parseIdentifier();
						// $FlowFixMe ColonMemberExpression is fucking up everything
						base = finishNode(ast.memberExpression(base, ".", identifier));
						break;
					case ":":
						pushLocation(marker);
						next();
						identifier = parseIdentifier();
						// $FlowFixMe ColonMemberExpression is fucking up everything
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

	//	   args ::= '(' [explist] ')' | tableconstructor | String

	function parseCallExpression(
		base
	): AST.CallExpression | AST.StringCallExpression | AST.TableCallExpression {
		if (Punctuator === token.type) {
			switch (token.value) {
				case "(": {
					if (!features.emptyStatement) {
						invariant(previousToken.type !== Placeholder);
						if (token.line !== previousToken.line)
							throw tokenError(errors.ambiguousSyntax, meta, token);
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
			const str = parsePrimaryExpression();
			invariant(str != null && str.type === "StringLiteral");
			return finishNode(ast.stringCallExpression(base, str));
		}

		throw tokenError(errors.expectedType, meta, token, "function arguments");
	}

	//	   primary ::= String | Numeric | nil | true | false
	//			| functiondef | tableconstructor | '...'
	// if identifier is true, it is parsing an identifier so does not
	// check for error with '...'
	function parsePrimaryExpression(
		identifier
	): ?(AST.Literal | AST.FunctionExpression | AST.TableConstructorExpression) {
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
				throw tokenError(errors.invalidVarargs, meta, token);
			pushLocation(marker);
			invariant(token.type !== Placeholder);
			const raw = input.slice(token.range[0], token.range[1]);
			next();
			return finishNode(ast.literal(type, value, raw));
		} else if (Keyword === type && "function" === value) {
			pushLocation(marker);
			next();
			createScope(true);
			const base = parseFunctionBase();
			return finishNode(ast.functionExpression(base));
		} else if (consume("{")) {
			pushLocation(marker);
			return parseTableConstructor();
		}
	}

	// Wrap up the node object.

	function finishNode<T>(node: T): T {
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

	function indexOf(array, element) {
		for (let i = 0, length = array.length; i < length; ++i) {
			if (array[i] === element) return i;
		}
		return -1;
	}

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

	function expect(value: string, expected?: string): void {
		if (value === token.value) next();
		// flowlint-next-line sketchy-null-string: off
		else if (expected)
			throw tokenError(errors.expectedType, meta, token, expected);
		else throw tokenError(errors.expectedValue, meta, token, value);
	}

	function isUnary(token) {
		if (Punctuator === token.type) return "#-~".indexOf(token.value) >= 0;
		if (Keyword === token.type) return "not" === token.value;
		return false;
	}

	// @TODO this needs to be rethought.
	function isCallExpression(
		expression: AST.Expression | AST.ColonMemberExpression
	): boolean {
		return (
			expression.type === "CallExpression" ||
			expression.type === "TableCallExpression" ||
			expression.type === "StringCallExpression"
		);
	}

	// Check if the token syntactically closes a block.

	function isBlockFollow(token) {
		if (Placeholder === token.type && token.value === "EOF") return true;
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

	function createLocationMarker() {
		return new Marker(token);
	}

	// Create a new `Marker` and add it to the FILO-array.
	function markLocation() {
		if (trackLocations) locations.push(createLocationMarker());
	}

	// Push an arbitrary `Marker` object onto the FILO-array.
	function pushLocation(marker: ?Marker) {
		if (trackLocations) {
			invariant(marker != null);
			locations.push(marker);
		}
	}

	const chunk = parseChunk();
	if (options.comments) chunk.comments = comments;

	if (locations.length > 0)
		throw new Error(
			"Location tracking failed. This is most likely a bug in luaparse"
		);

	return chunk;
}
