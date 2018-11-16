// @flow
import { ast, parse } from "./luaparse";

type TypeInfo = $ReadOnly<NodeTypeInfo>;

const nil_type: TypeInfo = Object.freeze(ast.typeInfo("nil"));
const any_type: TypeInfo = Object.freeze(ast.typeInfo("any"));
const number_type: TypeInfo = Object.freeze(ast.typeInfo("number"));
const string_type: TypeInfo = Object.freeze(ast.typeInfo("string"));
const boolean_type: TypeInfo = Object.freeze(ast.typeInfo("boolean"));
const table_type: TypeInfo = Object.freeze(ast.typeInfo("table"));
const function_type: TypeInfo = Object.freeze(ast.typeInfo("function"));

function testAssign(type1: TypeInfo, type2: TypeInfo): void {
  if (type1.value !== type2.value && type1.value !== "any")
    throw new Error("Cannot assign " + type2.value + " to " + type1.value);
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
): NodeChunk {
  const scopes: Array<{ [identifier: string]: ?TypeInfo }> = [];

  function createScope(): void {
    scopes.push({});
  }

  function destroyScope(): void {
    scopes.pop();
  }

  function assignType(
    var_: NodeIdentifier | NodeVarargLiteral,
    type: TypeInfo
  ): void {
    if (var_.type === "VarargLiteral") return;
    scopes[scopes.length - 1][var_.name] = type;
  }

  function getTypeFromScope(name: string): TypeInfo {
    for (let i = scopes.length - 1; i >= 0; i--)
      if (scopes[i][name]) return scopes[i][name];
    return any_type;
  }

  function readLiteral(node: NodeLiteral): TypeInfo {
    if (!node.type.endsWith("Literal") || node.type === "VarargLiteral")
      throw new Error("Invalid type");
    return literal_map[node.type];
  }

  function readVarargLiteral(node: NodeVarargLiteral): TypeInfo {
    // TODO: Properly deal with varargs
    return any_type;
  }

  function readBinaryExpressionType(
    node: NodeBinaryExpression | NodeLogicalExpression
  ): TypeInfo {
    const L = readExpression(node.left).value;
    const R = readExpression(node.right).value;
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
        return number_type;
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
        return boolean_type;
      case "==":
      case "~=":
        if (L !== "any" && R !== "any" && L !== R)
          throw new Error("Cannot compare values of different types");
        return boolean_type;
      case "and":
      case "or":
        // TODO: This should be the union of the types
        return any_type;
      case "..":
        if ((L !== "any" && L !== "string") || (R !== "any" && R !== "string"))
          throw new Error(`Cannot use '${node.operator}' with non-string`);
        return string_type;
      default:
        throw new Error("Unknown binary operation '" + node.operator + "'");
    }
  }

  function readUnaryExpression(node: NodeUnaryExpression): TypeInfo {
    const type = readExpression(node.argument).value;
    switch (node.operator) {
      case "-":
      case "~":
        if (type !== "any" && type !== "number")
          throw new Error(`Cannot use '${node.operator}' with non-number.`);
        return number_type;
      case "#":
        if (type !== "any" && type !== "table")
          throw new Error(`Cannot use '#' with non-table.`);
        return number_type;
      case "not":
        return boolean_type;
      default:
        throw new Error("Unknown unary operation '" + node.operator + "'");
    }
  }

  function readCallExpressionBase(
    node: NodeExpression | NodeColonMemberExpression
  ): TypeInfo {
    if (node.type === "MemberExpression" && node.indexer === ":") {
      const type = readExpression(node.base).value;
      if (type !== "any" && type !== "table")
        throw new Error("Can't index non-table.");
      return function_type;
    } else return readExpression(node);
  }

  function readCallExpression(
    node:
      | NodeCallExpression
      | NodeStringCallExpression
      | NodeTableCallExpression
  ): TypeInfo {
    const type = readCallExpressionBase(node.base).value;
    (node.arguments: $ReadOnlyArray<NodeExpression>).forEach(arg =>
      readExpression(arg)
    );
    if (type !== "any" && type !== "function")
      throw new Error("Cannot call non-function type.");
    return any_type;
  }

  function readTableConstructorExpression(
    node: NodeTableConstructorExpression
  ): TypeInfo {
    node.fields.forEach(field => {
      if (field.type === "TableValue") readExpression(field.value);
      else if (field.type === "TableKey") {
        readExpression(field.key);
        readExpression(field.value);
      } else if (field.type === "TableKeyString") readExpression(field.value);
      else throw new Error("Unknown TableConstructor field");
    });
    return table_type;
  }

  function readFunctionNamePrefix(
    node: NodeNonLocalFunctionNamePrefix
  ): TypeInfo {
    if (node.type === "Identifier") return getTypeFromScope(node.name);
    else {
      const type = readFunctionNamePrefix(node.base).value;
      if (type !== "any" && type !== "table")
        throw new Error("Can't index non-table.");
      return any_type;
    }
  }

  function readFunctionName(node: NodeNonLocalFunctionName): TypeInfo {
    if (node.type === "MemberExpression" && node.indexer === ":") {
      const type = readFunctionNamePrefix(node.base).value;
      if (type !== "any" && type !== "table")
        throw new Error("Can't index non-table.");
      return any_type;
    } else return readFunctionNamePrefix(node);
  }

  function readFunctionDeclaration(node: NodeFunctionDeclaration): TypeInfo {
    if (node.identifier != null) {
      if (node.isLocal)
        assignType(
          (node: NodeLocalNamedFunctionDeclaration).identifier,
          function_type
        );
      else {
        testAssign(readFunctionName(node.identifier), function_type);
      }
    }
    createScope();
    for (let i = 0; i < node.parameters.length; i++) {
      const type = node.parameter_types[i] ? node.parameter_types[i] : any_type;
      assignType(node.parameters[i], type);
    }
    readBlock(node.body);
    destroyScope();
    // Actually if this has an identifier the it is a statement and not
    // an expression, so maybe we should split those cases.
    return function_type;
  }

  function readIdentifier(node: NodeIdentifier): TypeInfo {
    return getTypeFromScope(node.name);
  }

  function readIndexExpression(node: NodeIndexExpression): TypeInfo {
    const type = readExpression(node.base).value;
    if (type !== "any" && type !== "table")
      throw new Error("Can't index non-table.");
    readExpression(node.index);
    return any_type;
  }

  function readMemberExpression(node: NodeMemberExpression): TypeInfo {
    const type = readExpression(node.base).value;
    if (type !== "any" && type !== "table")
      throw new Error("Can't index non-table.");
    return any_type;
  }

  function readExpression(node: NodeExpression): TypeInfo {
    if (node.type === "VarargLiteral") return readVarargLiteral(node);
    else if (
      node.type === "StringLiteral" ||
      node.type === "BooleanLiteral" ||
      node.type === "NumericLiteral" ||
      node.type === "NilLiteral"
    )
      return readLiteral(node);
    else if (node.type === "Identifier") return readIdentifier(node);
    else if (
      node.type === "BinaryExpression" ||
      node.type === "LogicalExpression"
    )
      return readBinaryExpressionType(node);
    else if (node.type === "UnaryExpression") return readUnaryExpression(node);
    else if (node.type === "CallExpression") return readCallExpression(node);
    else if (node.type === "StringCallExpression")
      return readCallExpression(node);
    else if (node.type === "TableCallExpression")
      return readCallExpression(node);
    else if (node.type === "TableConstructorExpression")
      return readTableConstructorExpression(node);
    else if (node.type === "FunctionDeclaration")
      return readFunctionDeclaration(node);
    else if (node.type === "MemberExpression")
      return readMemberExpression(node);
    else if (node.type === "IndexExpression") return readIndexExpression(node);
    else throw new Error(`Unknown Expression Type '${node.type}'`);
  }

  function readVariable(node: NodeVariable): TypeInfo {
    if (node.type === "Identifier") return readIdentifier(node);
    else if (node.type === "IndexExpression") return readIndexExpression(node);
    else if (node.type === "MemberExpression")
      return readMemberExpression(node);
    else throw new Error(`Unknow Variable Type '${node.type}'`);
  }

  function readLocalStatement(node: NodeLocalStatement): void {
    const n = Math.max(node.types.length, node.init.length);
    for (let i = 0; i < n; i++) {
      const type = node.types[i] ? node.types[i] : any_type,
        init_type = node.init[i] ? readExpression(node.init[i]) : nil_type;
      testAssign(type, init_type);
    }
    for (let i = 0; i < node.variables.length; i++) {
      const var_ = node.variables[i],
        type = node.types[i] || any_type;
      assignType(var_, type);
    }
  }

  function readAssignmentStatement(node: NodeAssignmentStatement): void {
    node.init.forEach(expr => readExpression(expr));
    for (let i = 0; i < node.variables.length; i++) {
      const type = readVariable(node.variables[i]);
      const init_type = node.init[i] ? readExpression(node.init[i]) : nil_type;
      testAssign(type, init_type);
    }
  }

  function readCallStatement(node: NodeCallStatement): void {
    readCallExpression(node.expression);
  }

  function readWhileStatement(node: NodeWhileStatement): void {
    const type = readExpression(node.condition).value;
    if (type !== "any" && type !== "boolean")
      throw new Error("While condition can't be non-boolean.");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  function readRepeatStatement(node: NodeRepeatStatement): void {
    createScope();
    readBlock(node.body);
    const type = readExpression(node.condition).value;
    if (type !== "any" && type !== "boolean")
      throw new Error("Repeat condition can't be non-boolean.");
    destroyScope();
  }

  function readGotoStatement(node: NodeGotoStatement): void {}

  function readLabelStatement(node: NodeLabelStatement): void {}

  function readReturnStatement(node: NodeReturnStatement): void {
    node.arguments.forEach(arg => readExpression(arg));
  }

  function readIfStatement(node: NodeIfStatement): void {
    node.clauses.forEach(clause => {
      if (clause.type !== "ElseClause") {
        const type = readExpression(clause.condition).value;
        if (type !== "any" && type !== "boolean")
          throw new Error("If condition can't be non-boolean.");
      }
      createScope();
      readBlock(clause.body);
      destroyScope();
    });
  }

  function readDoStatement(node: NodeDoStatement): void {
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  function readBreakStatement(node: NodeBreakStatement): void {}

  function readForNumericStatement(node: NodeForNumericStatement): void {
    const start_type = readExpression(node.start).value;
    const end_type = readExpression(node.end).value;
    const step_type =
      node.step != null ? readExpression(node.step).value : "any";
    if (
      (start_type !== "any" && start_type !== "number") ||
      (end_type !== "any" && end_type !== "number") ||
      (step_type !== "any" && step_type !== "number")
    )
      throw new Error("NumericFor limits should be integers");
    createScope();
    assignType(node.variable, number_type);
    readBlock(node.body);
    destroyScope();
  }

  function readForGenericStatement(node: NodeForGenericStatement): void {
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
    else if (node.type === "FunctionDeclaration") {
      readFunctionDeclaration(node);
      return;
    } else if (node.type === "GotoStatement") return readGotoStatement(node);
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
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  const ast = parse(code, options);
  readChunk(ast);
  return ast;
}
