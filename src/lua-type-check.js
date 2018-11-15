// @flow
import { ast, parse } from "./luaparse";

const nil_type = Object.freeze(ast.typeInfo("nil"));
const any_type = Object.freeze(ast.typeInfo("any"));
const number_type = Object.freeze(ast.typeInfo("number"));
const string_type = Object.freeze(ast.typeInfo("string"));
const boolean_type = Object.freeze(ast.typeInfo("boolean"));
const table_type = Object.freeze(ast.typeInfo("table"));
const function_type = Object.freeze(ast.typeInfo("function"));

function testAssign(type1, type2) {
  if (type1.type !== "TypeInfo" || type2.type !== "TypeInfo")
    throw new Error("Not TypeInfo");
  if (type1.value !== type2.value && type1.value !== "any") {
    throw new Error("Cannot assign " + type2.value + " to " + type1.value);
  }
}

function checkNodeType(node, type) {
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

  function createScope() {
    scopes.push({});
  }

  function destroyScope() {
    scopes.pop();
  }

  function assignType(var_, type) {
    if (var_.type === "VarargLiteral") return;
    checkNodeType(var_, "Identifier");
    scopes[scopes.length - 1][var_.name] = type;
  }

  function getTypeFromScope(name) {
    for (let i = scopes.length - 1; i >= 0; i--)
      if (scopes[i][name]) return scopes[i][name];
    return any_type;
  }

  function readLiteral(node) {
    if (!node.type.endsWith("Literal") || node.type === "VarargLiteral")
      throw new Error("Invalid type");
    node.expression_type = literal_map[node.type];
  }

  function readVarargLiteral(node) {
    checkNodeType(node, "VarargLiteral");
    // TODO: Properly deal with varargs
    node.expression_type = any_type;
  }

  function readBinaryExpressionType(node) {
    if (node.type !== "BinaryExpression" && node.type !== "LogicalExpression")
      throw new Error("wrong type");
    readExpression(node.left);
    readExpression(node.right);
    const L = node.left.expression_type.value;
    const R = node.right.expression_type.value;
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
        node.expression_type = number_type;
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
        node.expression_type = boolean_type;
        return;
      case "==":
      case "~=":
        if (L !== "any" && R !== "any" && L !== R)
          throw new Error("Cannot compare values of different types");
        node.expression_type = boolean_type;
        return;
      case "and":
      case "or":
        // TODO: This should be the union of the types
        node.expression_type = any_type;
        return;
      case "..":
        if ((L !== "any" && L !== "string") || (R !== "any" && R !== "string"))
          throw new Error(`Cannot use '${node.operator}' with non-string`);
        node.expression_type = string_type;
        return;
      default:
        throw new Error("Unknown binary operation '" + node.operator + "'");
    }
  }

  function readUnaryExpression(node) {
    checkNodeType(node, "UnaryExpression");
    readExpression(node.argument);
    const type = node.argument.expression_type.value;
    switch (node.operator) {
      case "-":
      case "~":
        if (type !== "any" && type !== "number")
          throw new Error(`Cannot use '${node.operator}' with non-number.`);
        node.expression_type = number_type;
        return;
      case "#":
        if (type !== "any" && type !== "table")
          throw new Error(`Cannot use '#' with non-table.`);
        node.expression_type = number_type;
        return;
      case "not":
        node.expression_type = boolean_type;
        return;
      default:
        throw new Error("Unknown unary operation '" + node.operator + "'");
    }
  }

  function readCallExpression(node) {
    if (
      node.type !== "CallExpression" &&
      node.type !== "StringCallExpression" &&
      node.type !== "TableCallExpression"
    )
      throw new Error("Incorrect type for call expression.");
    readExpression(node.base);
    node.arguments.forEach(arg => readExpression(arg));
    if (
      node.base.expression_type.value !== "any" &&
      node.base.expression_type.value !== "function"
    )
      throw new Error("Cannot call non-function type.");
    node.expression_type = any_type;
  }

  function readTableConstructorExpression(node) {
    checkNodeType(node, "TableConstructorExpression");
    node.fields.forEach(field => {
      if (field.type === "TableValue") readExpression(field.value);
      else if (field.type === "TableKey") {
        readExpression(field.key);
        readExpression(field.value);
      } else if (field.type === "TableKeyString") readExpression(field.value);
      else throw new Error("Unknown TableConstructor field");
    });
    node.expression_type = table_type;
  }

  function readFunctionDeclaration(node) {
    checkNodeType(node, "FunctionDeclaration");
    if (node.identifier != null) {
      if (node.isLocal) assignType(node.identifier, function_type);
      else {
        readVariable(node.identifier);
        testAssign(node.identifier.expression_type, function_type);
      }
    }
    createScope();
    for (let i = 0; i < node.parameters.length; i++) {
      const type = node.parameter_types[i] ? node.parameter_types[i] : any_type;
      assignType(node.parameters[i], type);
    }
    readBlock(node.body);
    destroyScope();
    if (node.identifier == null) node.expression_type = function_type;
  }

  function readIdentifier(node) {
    checkNodeType(node, "Identifier");
    node.expression_type = getTypeFromScope(node.name);
  }

  function readIndexExpression(node) {
    checkNodeType(node, "IndexExpression");
    readExpression(node.base);
    if (
      node.base.expression_type.value !== "any" &&
      node.base.expression_type.value !== "table"
    )
      throw new Error("Can't index non-table.");
    readExpression(node.index);
    node.expression_type = any_type;
  }

  function readMemberExpression(node) {
    checkNodeType(node, "MemberExpression");
    readExpression(node.base);
    if (
      node.base.expression_type.value !== "any" &&
      node.base.expression_type.value !== "table"
    )
      throw new Error("Can't index non-table.");
    node.expression_type = any_type;
  }

  function readExpression(node) {
    if (node == null) return;
    if (node.type === "VarargLiteral") readVarargLiteral(node);
    else if (node.type.endsWith("Literal") && node.type !== "VarargLiteral")
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

  function readVariable(node) {
    if (node.type === "Identifier") readIdentifier(node);
    else if (node.type === "IndexExpression") readIndexExpression(node);
    else if (node.type === "MemberExpression") readMemberExpression(node);
    else throw new Error(`Unknow Variable Type '${node.type}'`);
  }

  function readLocalStatement(node) {
    checkNodeType(node, "LocalStatement");
    const n = Math.max(node.types.length, node.init.length);
    for (let i = 0; i < n; i++) {
      if (node.init[i]) readExpression(node.init[i]);
      const type = node.types[i] ? node.types[i] : any_type,
        init_type = node.init[i] ? node.init[i].expression_type : nil_type;
      testAssign(type, init_type);
    }
    for (let i = 0; i < node.variables.length; i++) {
      const var_ = node.variables[i],
        type = node.types[i] || any_type;
      assignType(var_, type);
    }
  }

  function readAssignmentStatement(node) {
    checkNodeType(node, "AssignmentStatement");
    node.init.forEach(expr => readExpression(expr));
    for (let i = 0; i < node.variables.length; i++) {
      readVariable(node.variables[i]);
      const type = node.variables[i].expression_type;
      const init_type = node.init[i] ? node.init[i].expression_type : nil_type;
      testAssign(type, init_type);
    }
  }

  function readCallStatement(node) {
    checkNodeType(node, "CallStatement");
    readCallExpression(node.expression);
  }

  function readWhileStatement(node) {
    checkNodeType(node, "WhileStatement");
    readExpression(node.condition);
    if (
      node.condition.expression_type.value !== "any" &&
      node.condition.expression_type.value !== "boolean"
    )
      throw new Error("While condition can't be non-boolean.");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  function readRepeatStatement(node) {
    checkNodeType(node, "RepeatStatement");
    createScope();
    readBlock(node.body);
    readExpression(node.condition);
    if (
      node.condition.expression_type.value !== "any" &&
      node.condition.expression_type.value !== "boolean"
    )
      throw new Error("Repeat condition can't be non-boolean.");
    destroyScope();
  }

  function readGotoStatement(node) {
    checkNodeType(node, "GotoStatement");
  }

  function readLabelStatement(node) {
    checkNodeType(node, "LabelStatement");
  }

  function readReturnStatement(node) {
    checkNodeType(node, "ReturnStatement");
    node.arguments.forEach(arg => readExpression(arg));
  }

  function readIfStatement(node) {
    checkNodeType(node, "IfStatement");
    node.clauses.forEach(clause => {
      if (clause.type !== "ElseClause") {
        readExpression(clause.condition);
        if (
          clause.condition.expression_type.value !== "any" &&
          clause.condition.expression_type.value !== "boolean"
        )
          throw new Error("If condition can't be non-boolean.");
      }
      createScope();
      readBlock(clause.body);
      destroyScope();
    });
  }

  function readDoStatement(node) {
    checkNodeType(node, "DoStatement");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  function readBreakStatement(node) {
    checkNodeType(node, "BreakStatement");
  }

  function readForNumericStatement(node) {
    checkNodeType(node, "ForNumericStatement");
    readExpression(node.start);
    readExpression(node.end);
    if (node.step != null) readExpression(node.step);
    if (
      (node.start.expression_type.value != "any" &&
        node.start.expression_type.value != "number") ||
      (node.end.expression_type.value != "any" &&
        node.end.expression_type.value != "number") ||
      (node.step != null &&
        node.step.expression_type.value != "any" &&
        node.step.expression_type.value != "number")
    )
      throw new Error("NumericFor limits should be integers");
    createScope();
    assignType(node.variable, number_type);
    readBlock(node.body);
    destroyScope();
  }

  function readForGenericStatement(node) {
    checkNodeType(node, "ForGenericStatement");
    // TODO: deal properly with types here
    node.iterators.forEach(it => readExpression(it));
    createScope();
    node.variables.forEach(var_ => assignType(var_, any_type));
    readBlock(node.body);
    destroyScope();
  }

  function readStatement(node) {
    if (node.type === "LocalStatement") readLocalStatement(node);
    else if (node.type === "CallStatement") readCallStatement(node);
    else if (node.type === "WhileStatement") readWhileStatement(node);
    else if (node.type === "RepeatStatement") readRepeatStatement(node);
    else if (node.type === "AssignmentStatement")
      return readAssignmentStatement(node);
    else if (node.type === "FunctionDeclaration") readFunctionDeclaration(node);
    else if (node.type === "GotoStatement") readGotoStatement(node);
    else if (node.type === "LabelStatement") readLabelStatement(node);
    else if (node.type === "ReturnStatement") readReturnStatement(node);
    else if (node.type === "IfStatement") readIfStatement(node);
    else if (node.type === "DoStatement") readDoStatement(node);
    else if (node.type === "BreakStatement") readBreakStatement(node);
    else if (node.type === "ForNumericStatement") readForNumericStatement(node);
    else if (node.type === "ForGenericStatement") readForGenericStatement(node);
    else throw new Error(`Unknown Statement Type '${node.type}'`);
  }

  function readBlock(block) {
    block.forEach(node => readStatement(node));
  }

  function readChunk(node) {
    checkNodeType(node, "Chunk");
    createScope();
    readBlock(node.body);
    destroyScope();
  }

  // remove this : any
  const ast: any = parse(code, options);
  readChunk(ast);
  return ast;
}
