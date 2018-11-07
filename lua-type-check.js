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
  if (type1.value !== type2.value) {
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
    throw new Error("Name not found in any scope");
  }

  function readBinaryExpressionType(node) {
    checkNodeType(node, "BinaryExpression");
    readExpression(node.left);
    readExpression(node.right);
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
        if (
          node.left.expression_type.value !== "number" ||
          node.right.expression_type.value !== "number"
        )
          throw new Error(`Cannot use '${node.operator}' with non-number`);
        node.expression_type = number_type;
        return;
      case ">":
      case "<":
      case ">=":
      case "<=":
        if (
          node.left.expression_type.value !==
            node.right.expression_type.value ||
          (node.left.expression_type.value !== "number" &&
            node.left.expression_type.value !== "string")
        )
          throw new Error(
            `Cannot use '${node.operator}' with non-number or string.`
          );
        node.expression_type = boolean_type;
        return;
      case "==":
      case "~=":
        if (
          node.left.expression_type.value !== node.right.expression_type.value
        )
          throw new Error("Cannot compare values of different types");
        node.expression_type = boolean_type;
        return;
      case "and":
      case "or":
        if (
          node.left.expression_type.value !== "boolean" ||
          node.right.expression_type.value !== "boolean"
        )
          throw new Error(`Cannot use '${node.operator}' with non-boolean`);
        node.expression_type = boolean_type;
        return;
      case "..":
        if (
          node.left.expression_type.value !== "string" ||
          node.right.expression_type.value !== "string"
        )
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
    switch (node.operator) {
      case "-":
      case "~":
        if (node.argument.expression_type.value !== "number")
          throw new Error(`Cannot use '${node.operator}' with non-number.`);
        node.expression_type = number_type;
        return;
      case "#":
        if (node.argument.expression_type.value !== "table")
          throw new Error(`Cannot use '#' with non-table.`);
        node.expression_type = number_type;
        return;
      case "not":
        if (node.argument.expression_type.value !== "boolean")
          throw new Error(`Cannot use '#' with non-boolean.`);
        node.expression_type = boolean_type;
        return;
      default:
        throw new Error("Unknown unary operation '" + node.operator + "'");
    }
  }

  function readExpression(node) {
    if (node == null) return;
    if (node.type.endsWith("Literal") && node.type !== "VarargLiteral")
      node.expression_type = literal_map[node.type];
    else if (node.type === "Identifier")
      node.expression_type = getTypeFromScope(node.name);
    else if (node.type === "BinaryExpression") readBinaryExpressionType(node);
    else if (node.type === "UnaryExpression") readUnaryExpression(node);
    else throw new Error(`Unknown Expression Type '${node.type}'`);
  }

  function assignType(var_, type) {
    checkNodeType(var_, "Identifier");
    scopes[scopes.length - 1][var_.name] = type;
  }

  function readLocalStatement(node) {
    checkNodeType(node, "LocalStatement");
    for (let i = 0; i < node.types.length; i++) {
      readExpression(node.init[i]);
      const type = node.types[i],
        init_type = node.init[i] ? node.init[i].expression_type : nil_type;
      testAssign(type, init_type);
    }
    for (let i = 0; i < node.variables.length; i++) {
      const var_ = node.variables[i],
        type = node.types[i] || any_type;
      assignType(var_, type);
    }
  }

  function readStatement(node) {
    if (node.type === "LocalStatement") return readLocalStatement(node);
    throw new Error();
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
