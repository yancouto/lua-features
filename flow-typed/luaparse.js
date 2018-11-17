type LuaParseOptions = {|
  wait?: boolean,
  comments?: boolean,
  locations?: boolean,
  ranges?: boolean,
  extendedIdentifiers?: boolean,
  onCreateNode?: any => any,
  luaVersion?: "5.1" | "5.2" | "5.3" | "LuaJIT"
|};

// Example: "example"
type NodeStringLiteral = {|
  type: "StringLiteral",
  value: string,
  raw: string
|};

// Example: 1
type NodeNumericLiteral = {|
  type: "NumericLiteral",
  value: number,
  raw: string
|};

// Example: true
type NodeBooleanLiteral = {|
  type: "BooleanLiteral",
  value: boolean,
  raw: string
|};

// Example: ...
type NodeVarargLiteral = {|
  type: "VarargLiteral",
  value: "...",
  raw: string
|};

// Example: nil
type NodeNilLiteral = {|
  type: "NilLiteral",
  value: null,
  raw: string
|};

// Example: true
// Example: 12
type NodeLiteral =
  | NodeStringLiteral
  | NodeNumericLiteral
  | NodeBooleanLiteral
  | NodeVarargLiteral
  | NodeNilLiteral;

type NodeParenthesisExpression = {|
  type: "ParenthesisExpression",
  expression: NodeExpression
|};

// Example: 12 inside {12}
type NodeTableValue = {|
  type: "TableValue",
  value: NodeExpression
|};

// Example: [100]=1 inside {[100]=1}
type NodeTableKey = {|
  type: "TableKey",
  key: NodeExpression,
  value: NodeExpression
|};

// Example: test=1 inside {test=1}
type NodeTableKeyString = {|
  type: "TableKeyString",
  key: NodeIdentifier,
  value: NodeExpression
|};

// Example: {}
// Example: {a = 'b', 12, [function() end] = {}}
type NodeTableConstructorExpression = {|
  type: "TableConstructorExpression",
  fields: Array<NodeTableValue | NodeTableKey | NodeTableKeyString>
|};

// Example: :number inside local a:number = 1
type NodeTypeInfo = {|
  type: "TypeInfo",
  value: "number" | "string" | "boolean" | "nil" | "table" | "function" | "any"
|};

// Example: f inside f()
type NodeIdentifier = {|
  type: "Identifier",
  name: string
|};

// Example: not true
// Example: -12
type NodeUnaryExpression = {|
  type: "UnaryExpression",
  argument: NodeExpression,
  operator: "-" | "~" | "#" | "not"
|};

// Example: 1 + 2
type NodeBinaryExpression = {|
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
    | "~="
|};

// Example: a >= b or c >= d
type NodeLogicalExpression = {|
  type: "LogicalExpression",
  left: NodeExpression,
  right: NodeExpression,
  operator: "and" | "or"
|};

// Example: f()
// Example: g(1, 2, ...)
type NodeCallExpression = {|
  type: "CallExpression",
  base: NodeExpression | NodeColonMemberExpression,
  arguments: Array<NodeExpression>,
  hasVarargs: boolean
|};

// Example: f {}
// Example: f {1, oi=2}
type NodeTableCallExpression = {|
  type: "TableCallExpression",
  base: NodeExpression | NodeColonMemberExpression,
  arguments: [NodeTableConstructorExpression],
  hasVarargs: false
|};

// Example: f "test"
type NodeStringCallExpression = {|
  type: "StringCallExpression",
  base: NodeExpression | NodeColonMemberExpression,
  arguments: [NodeStringLiteral],
  hasVarargs: false
|};

// Example: function() end inside local f = function() end
type NodeUnnamedFunctionDeclaration = {|
  type: "FunctionDeclaration",
  identifier: null,
  isLocal: false,
  parameters: Array<NodeIdentifier>,
  hasVarargs: boolean,
  parameter_types: Array<NodeTypeInfo>,
  return_types: Array<NodeTypeInfo>,
  body: Array<NodeStatement>
|};

// Example: a.b inside a.b = 1
type NodeDotMemberExpression = {|
  type: "MemberExpression",
  base: NodeExpression,
  identifier: NodeIdentifier,
  indexer: "."
|};

// Example: a:b inside a:b()
// Example: a:b inside function a:b() end
type NodeColonMemberExpression = {|
  type: "MemberExpression",
  base: NodeExpression,
  identifier: NodeIdentifier,
  indexer: ":"
|};

// Example: a.b inside a.b = 1
// Example: a:b inside function a:b() end
type NodeMemberExpression = NodeColonMemberExpression | NodeDotMemberExpression;

// Function names are a bit tricky. The next types are for that.

type NodeNonLocalFunctionNamePrefix =
  | NodeIdentifier
  | NodeDotMemberExpressionFunctionName;

type NodeDotMemberExpressionFunctionName = {|
  type: "MemberExpression",
  base: NodeNonLocalFunctionNamePrefix,
  identifier: NodeIdentifier,
  indexer: "."
|};

type NodeColonMemberExpressionFunctionName = {|
  type: "MemberExpression",
  base: NodeNonLocalFunctionNamePrefix,
  identifier: NodeIdentifier,
  indexer: ":"
|};

type NodeNonLocalFunctionName =
  | NodeNonLocalFunctionNamePrefix
  | NodeColonMemberExpressionFunctionName;

// Example: function f() end
// Example: local function p(x) return x + 1 end
type NodeNonLocalNamedFunctionDeclaration = {|
  type: "FunctionDeclaration",
  identifier: NodeNonLocalFunctionName,
  isLocal: false,
  parameters: Array<NodeIdentifier>,
  hasVarargs: boolean,
  parameter_types: Array<NodeTypeInfo>,
  return_types: Array<NodeTypeInfo>,
  body: Array<NodeStatement>
|};

// Example: local function p(x) return x + 1 end
type NodeLocalNamedFunctionDeclaration = {|
  type: "FunctionDeclaration",
  identifier: NodeIdentifier,
  isLocal: true,
  parameters: Array<NodeIdentifier>,
  hasVarargs: boolean,
  parameter_types: Array<NodeTypeInfo>,
  return_types: Array<NodeTypeInfo>,
  body: Array<NodeStatement>
|};

// Example: function f() end
// Example: function(x) return x + 1 end inside local f = function(x) return x + 1 en
type NodeFunctionDeclaration =
  | NodeUnnamedFunctionDeclaration
  | NodeLocalNamedFunctionDeclaration
  | NodeNonLocalNamedFunctionDeclaration;

// Example: a["oi"] inside a["oi"] = 1
// Example: get("test")[function() end]
type NodeIndexExpression = {|
  type: "IndexExpression",
  base: NodeExpression,
  index: NodeExpression
|};

// Example: function() end inside local x = function() end
// Example: 1 + 2 * 3 + get() inside go(1 + 2 * 3 + get())
type NodeExpression =
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
type NodeLocalStatement = {|
  type: "LocalStatement",
  variables: Array<NodeIdentifier>,
  types: Array<NodeTypeInfo>,
  init: Array<NodeExpression>,
  hasVarargs: boolean
|};

// Example: a inside a = 1
// Example: a.b['test'] inside a.b['test'] = f()
type NodeVariable =
  | NodeIdentifier
  | NodeIndexExpression
  | NodeDotMemberExpression;

// Example: a = 1
// Example: a.b['test'] = f()
type NodeAssignmentStatement = {|
  type: "AssignmentStatement",
  variables: Array<NodeVariable>,
  init: Array<NodeExpression>,
  hasVarargs: boolean
|};

// Example: f()
// Example: g "test"
type NodeCallStatement = {|
  type: "CallStatement",
  expression:
    | NodeCallExpression
    | NodeStringCallExpression
    | NodeTableCallExpression
|};

// Example: while true do go() end
type NodeWhileStatement = {|
  type: "WhileStatement",
  condition: NodeExpression,
  body: Array<NodeStatement>
|};

// Example: repeat foo() until a() = b.c
type NodeRepeatStatement = {|
  type: "RepeatStatement",
  condition: NodeExpression,
  body: Array<NodeStatement>
|};

// Example: ::test::
type NodeLabelStatement = {|
  type: "LabelStatement",
  label: string
|};

// Example: goto test
type NodeGotoStatement = {|
  type: "GotoStatement",
  label: string
|};

// Example: break
type NodeBreakStatement = {|
  type: "BreakStatement"
|};

type NodeReturnStatement = {|
  type: "ReturnStatement",
  arguments: Array<NodeExpression>
|};

type NodeIfClause =
  | {|
      type: "IfClause" | "ElseifClause",
      condition: NodeExpression,
      body: Array<NodeStatement>
    |}
  | {|
      type: "ElseClause",
      body: Array<NodeStatement>
    |};

type NodeIfStatement = {|
  type: "IfStatement",
  clauses: Array<NodeIfClause>
|};

type NodeDoStatement = {|
  type: "DoStatement",
  body: Array<NodeStatement>
|};

type NodeForNumericStatement = {|
  type: "ForNumericStatement",
  variable: NodeIdentifier,
  start: NodeExpression,
  end: NodeExpression,
  step: ?NodeExpression,
  body: Array<NodeStatement>
|};

type NodeForGenericStatement = {|
  type: "ForGenericStatement",
  variables: Array<NodeIdentifier>,
  iterators: Array<NodeExpression>,
  body: Array<NodeStatement>
|};

type NodeStatement =
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

type NodeChunk = {|
  type: "Chunk",
  body: Array<NodeStatement>
|};

// this is a dirty trick to type local libs. Doesn't work if importing from a
// different path. A better solution is to make a wrapper typed lib
declare module "./luaparse" {
  declare export function parse(
    input: string,
    options: LuaParseOptions
  ): NodeChunk;
  declare export var ast: { typeInfo: string => NodeTypeInfo };
}
