"use strict";

const luaparse = require("./luaparse");

const nil_type = Object.freeze(luaparse.ast.typeInfo("nil"));
const any_type = Object.freeze(luaparse.ast.typeInfo("any"));

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
  StringLiteral: Object.freeze(luaparse.ast.typeInfo("string")),
  NumericLiteral: Object.freeze(luaparse.ast.typeInfo("number")),
  BooleanLiteral: Object.freeze(luaparse.ast.typeInfo("boolean"))
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

  function readExpressionType(node) {
    if (node == null) return nil_type;
    if (node.type.endsWith("Literal") && node.type !== "VarargLiteral")
      return literal_map[node.type];
    if (node.type === "Identifier") return getTypeFromScope(node.name);
    throw new Error();
  }

  function assignType(var_, type) {
    checkNodeType(var_, "Identifier");
    scopes[scopes.length - 1][var_.name] = type;
  }

  function readLocalStatement(node) {
    checkNodeType(node, "LocalStatement");
    for (let i = 0; i < node.types.length; i++) {
      const type = node.types[i],
        init_type = readExpressionType(node.init[i]);
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
