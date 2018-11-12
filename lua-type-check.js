"use strict";

const luaparse = require("./luaparse");

const nil_type = Object.freeze(luaparse.ast.typeInfo("nil"));
const any_type = Object.freeze(luaparse.ast.typeInfo("any"));
const number_type = Object.freeze(luaparse.ast.typeInfo("number"));
const string_type = Object.freeze(luaparse.ast.typeInfo("string"));
const boolean_type = Object.freeze(luaparse.ast.typeInfo("boolean"));

function testAssign(type1, type2) {
  console.log("testAssign", type1, type2);
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
  BooleanLiteral: boolean_type
});

module.exports.check = (code, options = {}) => {
  const scopes = [];

  function createScope() {
    scopes.push({});
  }

  function destroyScope() {
    scopes.pop();
  }

  function getTypeFromScope(name) {
    console.log("fromScope", name);
    for (let i = scopes.length - 1; i >= 0; i--)
      if (scopes[i][name]) return scopes[i][name];
    return any_type;
  }

  function readBinaryExpressionType(node) {
    checkNodeType(node, "BinaryExpression");
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
        if (
          (L !== "any" && L !== "boolean") ||
          (R !== "any" && R !== "boolean")
        )
          throw new Error(`Cannot use '${node.operator}' with non-boolean`);
        node.expression_type = boolean_type;
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
        if (type !== "any" && type !== "boolean")
          throw new Error(`Cannot use 'not' with non-boolean.`);
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

  function readExpression(node) {
    if (node == null) return;
    if (node.type.endsWith("Literal") && node.type !== "VarargLiteral")
      node.expression_type = literal_map[node.type];
    else if (node.type === "Identifier")
      node.expression_type = getTypeFromScope(node.name);
    else if (node.type === "BinaryExpression") readBinaryExpressionType(node);
    else if (node.type === "UnaryExpression") readUnaryExpression(node);
    else if (node.type === "CallExpression") readCallExpression(node);
    else if (node.type === "StringCallExpression") readCallExpression(node);
    else throw new Error(`Unknown Expression Type '${node.type}'`);
  }

  function assignType(var_, type) {
    checkNodeType(var_, "Identifier");
    scopes[scopes.length - 1][var_.name] = type;
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

  function readStatement(node) {
    if (node.type === "LocalStatement") return readLocalStatement(node);
    else if (node.type === "CallStatement") return readCallStatement(node);
    else if (node.type === "WhileStatement") return readWhileStatement(node);
    else if (node.type === "RepeatStatement") return readRepeatStatement(node);
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

  const ast = luaparse.parse(code, options);
  readChunk(ast);
  return ast;
};
