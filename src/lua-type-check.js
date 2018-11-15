// @flow
import { ast, parse } from "./luaparse";

const nil_type: $ReadOnly<NodeTypeInfo> = Object.freeze(ast.typeInfo("nil"));
const any_type: $ReadOnly<NodeTypeInfo> = Object.freeze(ast.typeInfo("any"));
const number_type: $ReadOnly<NodeTypeInfo> = Object.freeze(
  ast.typeInfo("number")
);
const string_type: $ReadOnly<NodeTypeInfo> = Object.freeze(
  ast.typeInfo("string")
);
const boolean_type: $ReadOnly<NodeTypeInfo> = Object.freeze(
  ast.typeInfo("boolean")
);
const table_type: $ReadOnly<NodeTypeInfo> = Object.freeze(
  ast.typeInfo("table")
);
const function_type: $ReadOnly<NodeTypeInfo> = Object.freeze(
  ast.typeInfo("function")
);

function testAssign(type1, type2) {
  if (type1.type !== "TypeInfo" || type2.type !== "TypeInfo")
    throw new Error("Not TypeInfo");
  if (type1.value !== type2.value && type1.value !== "any") {
    throw new Error("Cannot assign " + type2.value + " to " + type1.value);
  }
}

function checkNodeType(node, type): void {
  if (node.type !== type) throw new Error("Node type does not match " + type);
}

const literal_map = Object.freeze({
  StringLiteral: string_type,
  NumericLiteral: number_type,
  BooleanLiteral: boolean_type,
  NilLiteral: nil_type
});

export function check(
  code: string,
  options: LuaParseOptions = Object.freeze({})
): any {
  const scopes = [];

  function createScope(): void {
    scopes.push({});
  }

  function destroyScope(): void {
    scopes.pop();
  }

  function assignType(
    var_: NodeIdentifier | NodeVarargLiteral,
    type: $ReadOnly<NodeTypeInfo>
  ): void {
    if (var_.type === "VarargLiteral") return;
    checkNodeType(var_, "Identifier");
    scopes[scopes.length - 1][var_.name] = type;
  }

  function getTypeFromScope(name: string): $ReadOnly<NodeTypeInfo> {
    for (let i = scopes.length - 1; i >= 0; i--)
      if (scopes[i][name]) return scopes[i][name];
    return any_type;
  }

  function readLiteral(node: NodeLiteral): void {
    if (!node.type.endsWith("Literal") || node.type === "VarargLiteral")
      throw new Error("Invalid type");
    (node: any).expression_type = literal_map[node.type];
  }

  function readVarargLiteral(node: NodeVarargLiteral): void {
    checkNodeType(node, "VarargLiteral");
    // TODO: Properly deal with varargs
    (node: any).expression_type = any_type;
  }

  function readBinaryExpressionType(
    node: NodeBinaryExpression | NodeLogicalExpression
  ): void {
    if (node.type !== "BinaryExpression" && node.type !== "LogicalExpression")
      throw new Error("wrong type");
    readExpression(node.left);
    readExpression(node.right);
    const L = (node.left: any).expression_type.value;
    const R = (node.right: any).expression_type.value;
    switch (node.operator) {
      case "*":
      case "+":
      case "-":
      case "/":
      case "%":
      case "^":
      case ">>":
      case "<<":
      case "&":
      case "|":
      case "~":
      case "//":
        if ((L !== "any" && L !== "number") || (R !== "any" && R !== "number"))
          throw new Error(`Cannot use '${node.operator}' with non-number`);
        (node: any).expression_type = number_type;
        return;
      case ">":
      case "<":
      case ">=":
      case "<=":
        if (
          (L !== "any" && R !== "any" && L !== R) ||
          (L !== "any" && L !== "number" && L !== "string")
        )
          throw new Error(
            `Cannot use '${node.operator}' with non-number or string.`
          );
        (node: any).expression_type = boolean_type;
        return;
      case "==":
      case "~=":
        if (L !== "any" && R !== "any" && L !== R)
          throw new Error("Cannot compare values of different types");
        (node: any).expression_type = boolean_type;
        return;
      case "and":
      case "or":
        // TODO: This should be the union of the types
        (node: any).expression_type = any_type;
        return;
      case "..":
        if ((L !== "any" && L !== "string") || (R !== "any" && R !== "string"))
          throw new Error(`Cannot use '${node.operator}' with non-string`);
        (node: any).expression_type = string_type;
        return;
      default:
        throw new Error("Unknown binary operation '" + node.operator + "'");
    }
  }

  function readUnaryExpression(node: NodeUnaryExpression): void {
    checkNodeType(node, "UnaryExpression");
    readExpression(node.argument);
    const type = (node.argument: any).expression_type.value;
    switch (node.operator) {
      case "-":
      case "~":
        if (type !== "any" && type !== "number")
          throw new Error(`Cannot use '${node.operator}' with non-number.`);
        (node: any).expression_type = number_type;
        return;
      case "#":
        if (type !== "any" && type !== "table")
          throw new Error(`Cannot use '#' with non-table.`);
        (node: any).expression_type = number_type;
        return;
      case "not":
        (node: any).expression_type = boolean_type;
        return;
      default:
        throw new Error("Unknown unary operation '" + node.operator + "'");
    }
  }

  function readCallExpression(
    node:
      | NodeCallExpression
      | NodeStringCallExpression
      | NodeTableCallExpression
  ): void {
    if (
      node.type !== "CallExpression" &&
      node.type !== "StringCallExpression" &&
      node.type !== "TableCallExpression"
    )
      throw new Error("Incorrect type for call expression.");
    readExpression(node.base);
    (node.arguments: $ReadOnlyArray<NodeExpression>).forEach(arg =>
      readExpression(arg)
    );
    if (
      (node.base: any).expression_type.value !== "any" &&
      (node.base: any).expression_type.value !== "function"
    )
      throw new Error("Cannot call non-function type.");
    (node: any).expression_type = any_type;
  }

  function readTableConstructorExpression(
    node: NodeTableConstructorExpression
  ): void {
    checkNodeType(node, "TableConstructorExpression");
    node.fields.forEach(field => {
      if (field.type === "TableValue") readExpression(field.value);
      else if (field.type === "TableKey") {
        readExpression(field.key);
        readExpression(field.value);
      } else if (field.type === "TableKeyString") readExpression(field.value);
      else throw new Error("Unknown TableConstructor field");
    });
    (node: any).expression_type = table_type;
  }

  function readFunctionDeclaration(node: NodeFunctionDeclaration): void {
    checkNodeType(node, "FunctionDeclaration");
    if (node.identifier != null) {
      if (node.isLocal)
        assignType(
          (node: NodeLocalNamedFunctionDeclaration).identifier,
          function_type
        );
      else {
        readVariable((node.identifier: any));
        testAssign((node.identifier: any).expression_type, function_type);
      }
    }
    createScope();
    for (let i = 0; i < node.parameters.length; i++) {
      const type = node.parameter_types[i] ? node.parameter_types[i] : any_type;
      assignType(node.parameters[i], type);
    }
    readBlock(node.body);
    destroyScope();
    if (node.identifier == null) (node: any).expression_type = function_type;
  }

  function readIdentifier(node: NodeIdentifier): void {
    checkNodeType(node, "Identifier");
    (node: any).expression_type = getTypeFromScope(node.name);
  }

  function readIndexExpression(node: NodeIndexExpression): void {
    checkNodeType(node, "IndexExpression");
    readExpression(node.base);
    if (
      (node.base: any).expression_type.value !== "any" &&
      (node.base: any).expression_type.value !== "table"
    )
      throw new Error("Can't index non-table.");
    readExpression(node.index);
    (node: any).expression_type = any_type;
  }

  function readMemberExpression(node: NodeMemberExpression): void {
    checkNodeType(node, "MemberExpression");
    readExpression(node.base);
    if (
      (node.base: any).expression_type.value !== "any" &&
      (node.base: any).expression_type.value !== "table"
    )
      throw new Error("Can't index non-table.");
    (node: any).expression_type = any_type;
  }

  function readExpression(node: NodeExpression): void {
    if (node == null) return;
    if (node.type === "VarargLiteral") readVarargLiteral(node);
    else if (
      node.type === "StringLiteral" ||
      node.type === "BooleanLiteral" ||
      node.type === "NumericLiteral" ||
      node.type === "NilLiteral"
    )
      readLiteral(node);
    else if (node.type === "Identifier") readIdentifier(node);
    else if (
      node.type === "BinaryExpression" ||
      node.type === "LogicalExpression"
    )
      readBinaryExpressionType(node);
    else if (node.type === "UnaryExpression") readUnaryExpression(node);
    else if (node.type === "CallExpression") readCallExpression(node);
    else if (node.type === "StringCallExpression") readCallExpression(node);
    else if (node.type === "TableCallExpression") readCallExpression(node);
    else if (node.type === "TableConstructorExpression")
      readTableConstructorExpression(node);
    else if (node.type === "FunctionDeclaration") readFunctionDeclaration(node);
    else if (node.type === "MemberExpression") readMemberExpression(node);
    else if (node.type === "IndexExpression") readIndexExpression(node);
    else throw new Error(`Unknown Expression Type '${node.type}'`);
  }

  function readVariable(node: NodeVariable): void {
    if (node.type === "Identifier") return readIdentifier(node);
    else if (node.type === "IndexExpression") return readIndexExpression(node);
    else if (node.type === "MemberExpression")
      return readMemberExpression(node);
    else throw new Error(`Unknow Variable Type '${node.type}'`);
  }

  function readLocalStatement(node: NodeLocalStatement): void {
    checkNodeType(node, "LocalStatement");
    const n = Math.max(node.types.length, node.init.length);
    for (let i = 0; i < n; i++) {
      if (node.init[i]) readExpression(node.init[i]);
      const type = node.types[i] ? node.types[i] : any_type,
        init_type = node.init[i]
          ? (node.init[i]: any).expression_type
          : nil_type;
      testAssign(type, init_type);
    }
    for (let i = 0; i < node.variables.length; i++) {
      const var_ = node.variables[i],
        type = node.types[i] || any_type;
      assignType(var_, type);
    }
  }

  function readAssignmentStatement(node: NodeAssignmentStatement): void {
    checkNodeType(node, "AssignmentStatement");
    node.init.forEach(expr => readExpression(expr));
    for (let i = 0; i < node.variables.length; i++) {
      readVariable(node.variables[i]);
      const type = (node.variables[i]: any).expression_type;
      const init_type = node.init[i]
        ? (node.init[i]: any).expression_type
        : nil_type;
      testAssign(type, init_type);
    }
  }

  function readCallStatement(node: NodeCallStatement): void {
    checkNodeType(node, "CallStatement");
    readCallExpression(node.expression);
  }

  function readWhileStatement(node: NodeWhileStatement): void {
    checkNodeType(node, "WhileStatement");
    readExpression(node.condition);
    if (
      (node.condition: any).expression_type.value !== "any" &&
      (node.condition: any).expression_type.value !== "boolean"
    )
      throw new Error("While condition can't be non-boolean.");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  function readRepeatStatement(node: NodeRepeatStatement): void {
    checkNodeType(node, "RepeatStatement");
    createScope();
    readBlock(node.body);
    readExpression(node.condition);
    if (
      (node.condition: any).expression_type.value !== "any" &&
      (node.condition: any).expression_type.value !== "boolean"
    )
      throw new Error("Repeat condition can't be non-boolean.");
    destroyScope();
  }

  function readGotoStatement(node: NodeGotoStatement): void {
    checkNodeType(node, "GotoStatement");
  }

  function readLabelStatement(node: NodeLabelStatement): void {
    checkNodeType(node, "LabelStatement");
  }

  function readReturnStatement(node: NodeReturnStatement): void {
    checkNodeType(node, "ReturnStatement");
    node.arguments.forEach(arg => readExpression(arg));
  }

  function readIfStatement(node: NodeIfStatement): void {
    checkNodeType(node, "IfStatement");
    node.clauses.forEach(clause => {
      if (clause.type !== "ElseClause") {
        readExpression(clause.condition);
        if (
          (clause.condition: any).expression_type.value !== "any" &&
          (clause.condition: any).expression_type.value !== "boolean"
        )
          throw new Error("If condition can't be non-boolean.");
      }
      createScope();
      readBlock(clause.body);
      destroyScope();
    });
  }

  function readDoStatement(node: NodeDoStatement): void {
    checkNodeType(node, "DoStatement");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  function readBreakStatement(node: NodeBreakStatement): void {
    checkNodeType(node, "BreakStatement");
  }

  function readForNumericStatement(node: NodeForNumericStatement): void {
    checkNodeType(node, "ForNumericStatement");
    readExpression(node.start);
    readExpression(node.end);
    if (node.step != null) readExpression(node.step);
    if (
      ((node.start: any).expression_type.value != "any" &&
        (node.start: any).expression_type.value != "number") ||
      ((node.end: any).expression_type.value != "any" &&
        (node.end: any).expression_type.value != "number") ||
      (node.step != null &&
        (node.step: any).expression_type.value != "any" &&
        (node.step: any).expression_type.value != "number")
    )
      throw new Error("NumericFor limits should be integers");
    createScope();
    assignType(node.variable, number_type);
    readBlock(node.body);
    destroyScope();
  }

  function readForGenericStatement(node: NodeForGenericStatement): void {
    checkNodeType(node, "ForGenericStatement");
    // TODO: deal properly with types here
    node.iterators.forEach(it => readExpression(it));
    createScope();
    node.variables.forEach(var_ => assignType(var_, any_type));
    readBlock(node.body);
    destroyScope();
  }

  function readStatement(node: NodeStatement): void {
    if (node.type === "LocalStatement") return readLocalStatement(node);
    else if (node.type === "CallStatement") return readCallStatement(node);
    else if (node.type === "WhileStatement") return readWhileStatement(node);
    else if (node.type === "RepeatStatement") return readRepeatStatement(node);
    else if (node.type === "AssignmentStatement")
      return readAssignmentStatement(node);
    else if (node.type === "FunctionDeclaration")
      return readFunctionDeclaration(node);
    else if (node.type === "GotoStatement") return readGotoStatement(node);
    else if (node.type === "LabelStatement") return readLabelStatement(node);
    else if (node.type === "ReturnStatement") return readReturnStatement(node);
    else if (node.type === "IfStatement") return readIfStatement(node);
    else if (node.type === "DoStatement") return readDoStatement(node);
    else if (node.type === "BreakStatement") return readBreakStatement(node);
    else if (node.type === "ForNumericStatement")
      return readForNumericStatement(node);
    else if (node.type === "ForGenericStatement")
      return readForGenericStatement(node);
    else throw new Error(`Unknown Statement Type '${node.type}'`);
  }

  function readBlock(block: Array<NodeStatement>): void {
    block.forEach(node => readStatement(node));
  }

  function readChunk(node: NodeChunk): void {
    checkNodeType(node, "Chunk");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  const ast = parse(code, options);
  readChunk(ast);
  return ast;
}
