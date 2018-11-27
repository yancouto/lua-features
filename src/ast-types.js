// @flow strict

export type Position = {|
	line: number,
	column: number,
|};

export type LocationInfo = {|
	loc?: { start: Position, end: Position },
	range?: [number, number],
|};

export type Comment = {|
	+type: "Comment",
	+value: string,
	+raw: string,
	...LocationInfo,
|};

// Example: "example"
export type StringLiteral = {|
	+type: "StringLiteral",
	+value: string,
	+raw: string,
	...LocationInfo,
|};

// Example: 1
export type NumericLiteral = {|
	+type: "NumericLiteral",
	+value: number,
	+raw: string,
	...LocationInfo,
|};

// Example: true
export type BooleanLiteral = {|
	+type: "BooleanLiteral",
	+value: boolean,
	+raw: string,
	...LocationInfo,
|};

// Example: ...
export type VarargLiteral = {|
	+type: "VarargLiteral",
	+value: "...",
	+raw: string,
	...LocationInfo,
|};

// Example: nil
export type NilLiteral = {|
	+type: "NilLiteral",
	+value: null,
	+raw: string,
	...LocationInfo,
|};

// Example: true
// Example: 12
export type Literal =
	| StringLiteral
	| NumericLiteral
	| BooleanLiteral
	| VarargLiteral
	| NilLiteral;

export type ParenthesisExpression = {|
	+type: "ParenthesisExpression",
	+expression: Expression,
	...LocationInfo,
|};

// Example: 12 inside {12}
export type TableValue = {|
	+type: "TableValue",
	+value: Expression,
	...LocationInfo,
|};

// Example: [100]=1 inside {[100]=1}
export type TableKey = {|
	+type: "TableKey",
	+key: Expression,
	+value: Expression,
	...LocationInfo,
|};

// Example: test=1 inside {test=1}
export type TableKeyString = {|
	+type: "TableKeyString",
	+key: Identifier,
	+value: Expression,
	...LocationInfo,
|};

// Example: {}
// Example: {a = 'b', 12, [function() end] = {}}
export type TableConstructorExpression = {|
	+type: "TableConstructorExpression",
	+fields: Array<TableValue | TableKey | TableKeyString>,
	...LocationInfo,
|};

// Example: f inside f()
export type Identifier = {|
	+type: "Identifier",
	+name: string,
	...LocationInfo,
|};

// Example: not true
// Example: -12
export type UnaryExpression = {|
	+type: "UnaryExpression",
	+argument: Expression,
	+operator: "-" | "~" | "#" | "not",
	...LocationInfo,
|};

// Example: 1 + 2
export type BinaryExpression = {|
	+type: "BinaryExpression",
	+left: Expression,
	+right: Expression,
	+operator:
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
	...LocationInfo,
|};

// Example: a >= b or c >= d
export type LogicalExpression = {|
	+type: "LogicalExpression",
	+left: Expression,
	+right: Expression,
	+operator: "and" | "or",
	...LocationInfo,
|};

// Example: f()
// Example: g(1, 2, ...)
export type CallExpression = {|
	+type: "CallExpression",
	+base: Expression | ColonMemberExpression,
	+args: Array<Expression>,
	...LocationInfo,
|};

// Example: f {}
// Example: f {1, oi=2}
export type TableCallExpression = {|
	+type: "TableCallExpression",
	+base: Expression | ColonMemberExpression,
	+args: [TableConstructorExpression],
	...LocationInfo,
|};

// Example: f "test"
export type StringCallExpression = {|
	+type: "StringCallExpression",
	+base: Expression | ColonMemberExpression,
	+args: [StringLiteral],
	...LocationInfo,
|};

// Example: function() end inside local f = function() end
export type UnnamedFunctionDeclaration = {|
	+type: "FunctionDeclaration",
	+identifier: null,
	+hasVarargs: boolean,
	+isLocal: false,
	+parameters: Array<Identifier>,
	+parameter_types: TypeList,
	+return_types: TypeList,
	+body: Block,
	...LocationInfo,
|};

// Example: a.b inside a.b = 1
export type DotMemberExpression = {|
	+type: "MemberExpression",
	+base: Expression,
	+identifier: Identifier,
	+indexer: ".",
	...LocationInfo,
|};

// Example: a:b inside a:b()
// Example: a:b inside function a:b() end
export type ColonMemberExpression = {|
	+type: "MemberExpression",
	+base: Expression,
	+identifier: Identifier,
	+indexer: ":",
	...LocationInfo,
|};

// Example: a.b inside a.b = 1
// Example: a:b inside function a:b() end
export type MemberExpression = ColonMemberExpression | DotMemberExpression;

// Function names are a bit tricky. The next types are for that.

export type NonLocalFunctionNamePrefix =
	| Identifier
	| DotMemberExpressionFunctionName;

export type DotMemberExpressionFunctionName = {|
	+type: "MemberExpression",
	+base: NonLocalFunctionNamePrefix,
	+identifier: Identifier,
	+indexer: ".",
	...LocationInfo,
|};

export type ColonMemberExpressionFunctionName = {|
	+type: "MemberExpression",
	+base: NonLocalFunctionNamePrefix,
	+identifier: Identifier,
	+indexer: ":",
	...LocationInfo,
|};

export type NonLocalFunctionName =
	| NonLocalFunctionNamePrefix
	| ColonMemberExpressionFunctionName;

// Example: function f() end
// Example: local function p(x) return x + 1 end
export type NonLocalNamedFunctionDeclaration = {|
	+type: "FunctionDeclaration",
	+identifier: NonLocalFunctionName,
	+hasVarargs: boolean,
	+isLocal: false,
	+parameters: Array<Identifier>,
	+parameter_types: TypeList,
	+return_types: TypeList,
	+body: Block,
	...LocationInfo,
|};

// Example: local function p(x) return x + 1 end
export type LocalNamedFunctionDeclaration = {|
	+type: "FunctionDeclaration",
	+identifier: Identifier,
	+hasVarargs: boolean,
	+isLocal: true,
	+parameters: Array<Identifier>,
	+parameter_types: TypeList,
	+return_types: TypeList,
	+body: Block,
	...LocationInfo,
|};

// Example: function f() end
// Example: function(x) return x + 1 end inside local f = function(x) return x + 1 en
export type FunctionDeclaration =
	| UnnamedFunctionDeclaration
	| LocalNamedFunctionDeclaration
	| NonLocalNamedFunctionDeclaration;

// Example: a["oi"] inside a["oi"] = 1
// Example: get("test")[function() end]
export type IndexExpression = {|
	+type: "IndexExpression",
	+base: Expression,
	+index: Expression,
	...LocationInfo,
|};

// Example: function() end inside local x = function() end
// Example: 1 + 2 * 3 + get() inside go(1 + 2 * 3 + get())
export type Expression =
	| Identifier
	| Literal
	| ParenthesisExpression
	| BinaryExpression
	| LogicalExpression
	| UnaryExpression
	| CallExpression
	| StringCallExpression
	| TableCallExpression
	| TableConstructorExpression
	| UnnamedFunctionDeclaration
	| DotMemberExpression
	| IndexExpression;

// Example: local x, y : number, string = 1, "test"
export type LocalStatement = {|
	+type: "LocalStatement",
	+variables: Array<Identifier>,
	+typeList: TypeList,
	+init: Array<Expression>,
	...LocationInfo,
|};

// Example: a inside a = 1
// Example: a.b['test'] inside a.b['test'] = f()
export type Variable = Identifier | IndexExpression | DotMemberExpression;

// Example: a = 1
// Example: a.b['test'] = f()
export type AssignmentStatement = {|
	+type: "AssignmentStatement",
	+variables: Array<Variable>,
	+init: Array<Expression>,
	...LocationInfo,
|};

// Example: f()
// Example: g "test"
export type CallStatement = {|
	+type: "CallStatement",
	+expression: CallExpression | StringCallExpression | TableCallExpression,
	...LocationInfo,
|};

// Example: while true do go() end
export type WhileStatement = {|
	+type: "WhileStatement",
	+condition: Expression,
	+body: Block,
	...LocationInfo,
|};

// Example: repeat foo() until a() = b.c
export type RepeatStatement = {|
	+type: "RepeatStatement",
	+condition: Expression,
	+body: Block,
	...LocationInfo,
|};

// Example: ::test::
export type LabelStatement = {|
	+type: "LabelStatement",
	+label: Identifier,
	...LocationInfo,
|};

// Example: goto test
export type GotoStatement = {|
	+type: "GotoStatement",
	+label: Identifier,
	...LocationInfo,
|};

// Example: break
export type BreakStatement = {|
	+type: "BreakStatement",
	...LocationInfo,
|};

export type ReturnStatement = {|
	+type: "ReturnStatement",
	+args: Array<Expression>,
	...LocationInfo,
|};

export type IfClause = {|
	+type: "IfClause",
	+condition: Expression,
	+body: Block,
	...LocationInfo,
|};

export type ElseifClause = {|
	+type: "ElseifClause",
	+condition: Expression,
	+body: Block,
	...LocationInfo,
|};

export type ElseClause = {|
	+type: "ElseClause",
	+body: Block,
	...LocationInfo,
|};

export type IfStatement = {|
	+type: "IfStatement",
	+clauses: Array<IfClause | ElseifClause | ElseClause>,
	...LocationInfo,
|};

export type DoStatement = {|
	+type: "DoStatement",
	+body: Block,
	...LocationInfo,
|};

export type ForNumericStatement = {|
	+type: "ForNumericStatement",
	+variable: Identifier,
	+start: Expression,
	+end: Expression,
	+step: ?Expression,
	+body: Block,
	...LocationInfo,
|};

export type ForGenericStatement = {|
	+type: "ForGenericStatement",
	+variables: Array<Identifier>,
	+iterators: Array<Expression>,
	+body: Block,
	...LocationInfo,
|};

export type Statement =
	| LocalStatement
	| CallStatement
	| WhileStatement
	| RepeatStatement
	| AssignmentStatement
	| LocalNamedFunctionDeclaration
	| NonLocalNamedFunctionDeclaration
	| GotoStatement
	| LabelStatement
	| ReturnStatement
	| IfStatement
	| DoStatement
	| BreakStatement
	| ForNumericStatement
	| ForGenericStatement;

export type Block = $ReadOnlyArray<Statement>;

export type Chunk = {|
	+type: "Chunk",
	+body: Block,
	+comments?: Array<Comment>,
	...LocationInfo,
|};

// TYPE STUFF

// Example: :number inside local a:number = 1
export type SimpleType = {|
	+type: "SimpleType",
	+value:
		| "number"
		| "string"
		| "boolean"
		| "nil"
		| "table"
		| "function"
		| "any",
|};

export type FunctionType = {|
	+type: "FunctionType",
	+parameter_types: TypeList,
	+return_types: TypeList,
|};

export type TableType = {|
	+type: "TableType",
	+typeMap: Map<string, TypeInfo>,
|};

export type SingleType = SimpleType | FunctionType | TableType;

export type TypeInfo = {|
	+type: "TypeInfo",
	+possibleTypes: Set<SingleType>,
|};

export type TypeList = {
	+type: "TypeList",
	+list: Array<TypeInfo>,
	+rest: TypeInfo,
};
