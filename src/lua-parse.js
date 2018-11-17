// @flow

// This is a wrapper to type the luaparse library while flow is not added
// to it :)
import { ast as untyped_ast, parse as untyped_parse } from "./luaparse";

export type LuaParseOptions = {|
	wait?: boolean,
	comments?: boolean,
	locations?: boolean,
	ranges?: boolean,
	extendedIdentifiers?: boolean,
	onCreateNode?: any => any,
	luaVersion?: "5.1" | "5.2" | "5.3" | "LuaJIT",
|};

// Example: "example"
export type NodeStringLiteral = {|
	type: "StringLiteral",
	value: string,
	raw: string,
|};

// Example: 1
export type NodeNumericLiteral = {|
	type: "NumericLiteral",
	value: number,
	raw: string,
|};

// Example: true
export type NodeBooleanLiteral = {|
	type: "BooleanLiteral",
	value: boolean,
	raw: string,
|};

// Example: ...
export type NodeVarargLiteral = {|
	type: "VarargLiteral",
	value: "...",
	raw: string,
|};

// Example: nil
export type NodeNilLiteral = {|
	type: "NilLiteral",
	value: null,
	raw: string,
|};

// Example: true
// Example: 12
export type NodeLiteral =
	| NodeStringLiteral
	| NodeNumericLiteral
	| NodeBooleanLiteral
	| NodeVarargLiteral
	| NodeNilLiteral;

export type NodeParenthesisExpression = {|
	type: "ParenthesisExpression",
	expression: NodeExpression,
|};

// Example: 12 inside {12}
export type NodeTableValue = {|
	type: "TableValue",
	value: NodeExpression,
|};

// Example: [100]=1 inside {[100]=1}
export type NodeTableKey = {|
	type: "TableKey",
	key: NodeExpression,
	value: NodeExpression,
|};

// Example: test=1 inside {test=1}
export type NodeTableKeyString = {|
	type: "TableKeyString",
	key: NodeIdentifier,
	value: NodeExpression,
|};

// Example: {}
// Example: {a = 'b', 12, [function() end] = {}}
export type NodeTableConstructorExpression = {|
	type: "TableConstructorExpression",
	fields: Array<NodeTableValue | NodeTableKey | NodeTableKeyString>,
|};

// Example: f inside f()
export type NodeIdentifier = {|
	type: "Identifier",
	name: string,
|};

// Example: not true
// Example: -12
export type NodeUnaryExpression = {|
	type: "UnaryExpression",
	argument: NodeExpression,
	operator: "-" | "~" | "#" | "not",
|};

// Example: 1 + 2
export type NodeBinaryExpression = {|
	type: "BinaryExpression",
	left: NodeExpression,
	right: NodeExpression,
	operator:
		| "*"
		| "+"
		| "-"
		| "/"
		| "%"
		| "^"
		| ">>"
		| "<<"
		| "&"
		| "|"
		| "~"
		| "//"
		| ".."
		| ">"
		| ">="
		| "<"
		| "<="
		| "=="
		| "~=",
|};

// Example: a >= b or c >= d
export type NodeLogicalExpression = {|
	type: "LogicalExpression",
	left: NodeExpression,
	right: NodeExpression,
	operator: "and" | "or",
|};

// Example: f()
// Example: g(1, 2, ...)
export type NodeCallExpression = {|
	type: "CallExpression",
	base: NodeExpression | NodeColonMemberExpression,
	arguments: Array<NodeExpression>,
	hasVarargs: boolean,
|};

// Example: f {}
// Example: f {1, oi=2}
export type NodeTableCallExpression = {|
	type: "TableCallExpression",
	base: NodeExpression | NodeColonMemberExpression,
	arguments: [NodeTableConstructorExpression],
	hasVarargs: false,
|};

// Example: f "test"
export type NodeStringCallExpression = {|
	type: "StringCallExpression",
	base: NodeExpression | NodeColonMemberExpression,
	arguments: [NodeStringLiteral],
	hasVarargs: false,
|};

// Example: function() end inside local f = function() end
export type NodeUnnamedFunctionDeclaration = {|
	type: "FunctionDeclaration",
	identifier: null,
	isLocal: false,
	parameters: Array<NodeIdentifier>,
	hasVarargs: boolean,
	parameter_types: TypeList,
	return_types: TypeList,
	body: Array<NodeStatement>,
|};

// Example: a.b inside a.b = 1
export type NodeDotMemberExpression = {|
	type: "MemberExpression",
	base: NodeExpression,
	identifier: NodeIdentifier,
	indexer: ".",
|};

// Example: a:b inside a:b()
// Example: a:b inside function a:b() end
export type NodeColonMemberExpression = {|
	type: "MemberExpression",
	base: NodeExpression,
	identifier: NodeIdentifier,
	indexer: ":",
|};

// Example: a.b inside a.b = 1
// Example: a:b inside function a:b() end
export type NodeMemberExpression =
	| NodeColonMemberExpression
	| NodeDotMemberExpression;

// Function names are a bit tricky. The next types are for that.

export type NodeNonLocalFunctionNamePrefix =
	| NodeIdentifier
	| NodeDotMemberExpressionFunctionName;

export type NodeDotMemberExpressionFunctionName = {|
	type: "MemberExpression",
	base: NodeNonLocalFunctionNamePrefix,
	identifier: NodeIdentifier,
	indexer: ".",
|};

export type NodeColonMemberExpressionFunctionName = {|
	type: "MemberExpression",
	base: NodeNonLocalFunctionNamePrefix,
	identifier: NodeIdentifier,
	indexer: ":",
|};

export type NodeNonLocalFunctionName =
	| NodeNonLocalFunctionNamePrefix
	| NodeColonMemberExpressionFunctionName;

// Example: function f() end
// Example: local function p(x) return x + 1 end
export type NodeNonLocalNamedFunctionDeclaration = {|
	type: "FunctionDeclaration",
	identifier: NodeNonLocalFunctionName,
	isLocal: false,
	parameters: Array<NodeIdentifier>,
	hasVarargs: boolean,
	parameter_types: TypeList,
	return_types: TypeList,
	body: Array<NodeStatement>,
|};

// Example: local function p(x) return x + 1 end
export type NodeLocalNamedFunctionDeclaration = {|
	type: "FunctionDeclaration",
	identifier: NodeIdentifier,
	isLocal: true,
	parameters: Array<NodeIdentifier>,
	hasVarargs: boolean,
	parameter_types: TypeList,
	return_types: TypeList,
	body: Array<NodeStatement>,
|};

// Example: function f() end
// Example: function(x) return x + 1 end inside local f = function(x) return x + 1 en
export type NodeFunctionDeclaration =
	| NodeUnnamedFunctionDeclaration
	| NodeLocalNamedFunctionDeclaration
	| NodeNonLocalNamedFunctionDeclaration;

// Example: a["oi"] inside a["oi"] = 1
// Example: get("test")[function() end]
export type NodeIndexExpression = {|
	type: "IndexExpression",
	base: NodeExpression,
	index: NodeExpression,
|};

// Example: function() end inside local x = function() end
// Example: 1 + 2 * 3 + get() inside go(1 + 2 * 3 + get())
export type NodeExpression =
	| NodeIdentifier
	| NodeLiteral
	| NodeParenthesisExpression
	| NodeBinaryExpression
	| NodeLogicalExpression
	| NodeUnaryExpression
	| NodeCallExpression
	| NodeStringCallExpression
	| NodeTableCallExpression
	| NodeTableConstructorExpression
	| NodeUnnamedFunctionDeclaration
	| NodeDotMemberExpression
	| NodeIndexExpression;

// Example: local x, y : number, string = 1, "test"
export type NodeLocalStatement = {|
	type: "LocalStatement",
	variables: Array<NodeIdentifier>,
	types: TypeList,
	init: Array<NodeExpression>,
	hasVarargs: boolean,
|};

// Example: a inside a = 1
// Example: a.b['test'] inside a.b['test'] = f()
export type NodeVariable =
	| NodeIdentifier
	| NodeIndexExpression
	| NodeDotMemberExpression;

// Example: a = 1
// Example: a.b['test'] = f()
export type NodeAssignmentStatement = {|
	type: "AssignmentStatement",
	variables: Array<NodeVariable>,
	init: Array<NodeExpression>,
	hasVarargs: boolean,
|};

// Example: f()
// Example: g "test"
export type NodeCallStatement = {|
	type: "CallStatement",
	expression:
		| NodeCallExpression
		| NodeStringCallExpression
		| NodeTableCallExpression,
|};

// Example: while true do go() end
export type NodeWhileStatement = {|
	type: "WhileStatement",
	condition: NodeExpression,
	body: Array<NodeStatement>,
|};

// Example: repeat foo() until a() = b.c
export type NodeRepeatStatement = {|
	type: "RepeatStatement",
	condition: NodeExpression,
	body: Array<NodeStatement>,
|};

// Example: ::test::
export type NodeLabelStatement = {|
	type: "LabelStatement",
	label: string,
|};

// Example: goto test
export type NodeGotoStatement = {|
	type: "GotoStatement",
	label: string,
|};

// Example: break
export type NodeBreakStatement = {|
	type: "BreakStatement",
|};

export type NodeReturnStatement = {|
	type: "ReturnStatement",
	arguments: Array<NodeExpression>,
|};

export type NodeIfClause =
	| {|
			type: "IfClause" | "ElseifClause",
			condition: NodeExpression,
			body: Array<NodeStatement>,
	  |}
	| {|
			type: "ElseClause",
			body: Array<NodeStatement>,
	  |};

export type NodeIfStatement = {|
	type: "IfStatement",
	clauses: Array<NodeIfClause>,
|};

export type NodeDoStatement = {|
	type: "DoStatement",
	body: Array<NodeStatement>,
|};

export type NodeForNumericStatement = {|
	type: "ForNumericStatement",
	variable: NodeIdentifier,
	start: NodeExpression,
	end: NodeExpression,
	step: ?NodeExpression,
	body: Array<NodeStatement>,
|};

export type NodeForGenericStatement = {|
	type: "ForGenericStatement",
	variables: Array<NodeIdentifier>,
	iterators: Array<NodeExpression>,
	body: Array<NodeStatement>,
|};

export type NodeStatement =
	| NodeLocalStatement
	| NodeCallStatement
	| NodeWhileStatement
	| NodeRepeatStatement
	| NodeAssignmentStatement
	| NodeLocalNamedFunctionDeclaration
	| NodeNonLocalNamedFunctionDeclaration
	| NodeGotoStatement
	| NodeLabelStatement
	| NodeReturnStatement
	| NodeIfStatement
	| NodeDoStatement
	| NodeBreakStatement
	| NodeForNumericStatement
	| NodeForGenericStatement;

export type NodeChunk = {|
	type: "Chunk",
	body: Array<NodeStatement>,
|};

// TYPE STUFF

// Example: :number inside local a:number = 1
export type NodeSimpleType = {|
	type: "SimpleType",
	value: "number" | "string" | "boolean" | "nil" | "table" | "function" | "any",
|};

export type NodeFunctionType = {|
	type: "FunctionType",
	parameter_types: TypeList,
	return_types: TypeList,
|};

export type NodeTypeInfo = NodeSimpleType | NodeFunctionType;

// null here means there was no type declaration
// (it should be considered as infinite any's)
// while [] means the user explicitly said it was : void or declared a
// function with no arguments
export type TypeList = ?TypeList;

export const ast: {
	simpleType: string => NodeSimpleType,
	functionType: (TypeList, TypeList) => NodeFunctionType,
} = untyped_ast;
export const parse: (string, LuaParseOptions) => NodeChunk = untyped_parse;
