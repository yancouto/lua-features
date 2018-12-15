// @flow strict-local
import * as AST from "./ast-types";

import { ast, parse, parseFile } from "./lua-parse";
import fs from "fs";

import invariant from "assert";
import type { LuaParseOptions } from "./lua-parse";

const nil_single = ast.simpleType("nil");
const any_single = ast.simpleType("any");
const number_single = ast.simpleType("number");
const string_single = ast.simpleType("string");
const boolean_single = ast.simpleType("boolean");
const table_single = ast.simpleType("table");
const function_single = ast.simpleType("function");

const nil_type = ast.typeInfo(new Set([nil_single]));
const any_type = ast.typeInfo(new Set([any_single]));
const number_type = ast.typeInfo(new Set([number_single]));
const string_type = ast.typeInfo(new Set([string_single]));
const boolean_type = ast.typeInfo(new Set([boolean_single]));
const table_type = ast.typeInfo(new Set([table_single]));
const function_type = ast.typeInfo(new Set([function_single]));

function isSupertype(sup: AST.TypeInfo, sub: AST.TypeInfo): boolean {
	return [...sub.possibleTypes].every(sub_type =>
		[...sup.possibleTypes].some(sup_type =>
			isSupertypeSingle(sup_type, sub_type)
		)
	);
}

function isSupertypeSingle(sup: AST.SingleType, sub: AST.SingleType): boolean {
	if (sup.type === "SimpleType" && sup.value === "any") return true;
	// should this be allowed?
	if (sub.type === "SimpleType" && sub.value === "any") return true;
	if (sub.type === "SimpleType" && sup.type === "SimpleType")
		return sub.value === sup.value;
	if (
		sup.type === "SimpleType" &&
		sup.value === "function" &&
		sub.type === "FunctionType"
	)
		return true;
	if (
		sup.type === "SimpleType" &&
		sup.value === "table" &&
		sub.type === "TableType"
	)
		return true;
	if (sub.type === "FunctionType" && sup.type === "FunctionType")
		return (
			isSupertypeList(sup.parameter_types, sub.parameter_types) &&
			isSupertypeList(sup.return_types, sub.return_types)
		);
	if (sub.type === "TableType" && sup.type === "TableType")
		return [...sub.typeMap.keys(), ...sup.typeMap.keys()].every(name =>
			isSupertype(
				sup.typeMap.get(name) || nil_type,
				sub.typeMap.get(name) || nil_type
			)
		);

	return false;
}

function isSupertypeList(sup: AST.TypeList, sub: AST.TypeList): boolean {
	const n = Math.max(sup.list.length, sub.list.length);
	for (let i = 0; i < n; i++) {
		const sup_type = getType(sup, i);
		const sub_type = getType(sub, i);
		if (!isSupertype(sup_type, sub_type)) return false;
	}
	return isSupertype(sup.rest, sub.rest);
}

function assertAssign(a: AST.TypeInfo, b: AST.TypeInfo): void {
	if (!isSupertype(a, b))
		throw new Error(
			`Can't assign "${typeToString(b)}" to "${typeToString(a)}".`
		);
}

function singleToType(a: AST.SingleType): AST.TypeInfo {
	return ast.typeInfo(new Set([a]));
}

function isSimple(t: AST.TypeInfo, value: string): boolean {
	return [...t.possibleTypes].every(
		single =>
			single.type === "SimpleType" &&
			(single.value === value || single.value === "any")
	);
}

function isSameSimple(t1: AST.TypeInfo, t2: AST.TypeInfo): boolean {
	return [...t1.possibleTypes].every(
		a =>
			a.type === "SimpleType" &&
			(a.value === "any" ||
				[...t2.possibleTypes].every(
					b =>
						b.type === "SimpleType" &&
						(b.value === "any" || a.value === b.value)
				))
	);
}

function isAny(t: AST.TypeInfo): boolean {
	return isSimple(t, "any");
}

function isNumber(t: AST.TypeInfo): boolean {
	return isSimple(t, "number");
}

function isString(t: AST.TypeInfo): boolean {
	return isSimple(t, "string");
}

function isBoolean(t: AST.TypeInfo): boolean {
	return isSimple(t, "boolean");
}

function isTable(t: AST.TypeInfo): boolean {
	return isSimple(t, "table");
}

function isFunction(t: AST.TypeInfo): boolean {
	return isSimple(t, "function");
}

function getType(tl: AST.TypeList, i: number): AST.TypeInfo {
	if (i < tl.list.length) return tl.list[i];
	else return tl.rest;
}

function firstType(tl: AST.TypeList): AST.TypeInfo {
	return getType(tl, 0);
}

function joinTypes(...ts: $ReadOnlyArray<AST.TypeInfo>): AST.TypeInfo {
	return ast.typeInfo(new Set([].concat(...ts.map(t => [...t.possibleTypes]))));
}

function typeToString(t: AST.TypeInfo): string {
	return [...t.possibleTypes]
		.map(single => singleTypeToString(single))
		.join(" | ");
}

function singleTypeToString(t: AST.SingleType): string {
	if (t.type === "SimpleType") return t.value;
	else if (t.type === "FunctionType")
		return `(${typeListToString(t.parameter_types)}) => (${typeListToString(
			t.return_types
		)})`;
	else if (t.type === "TableType")
		return `{${[...t.typeMap.entries()]
			.map(([k, v]) => `${k}: ${typeToString(v)}`)
			.join(", ")}}`;
	throw new Error(`Unknow AST.TypeInfo type '${t.type}'`);
}

function isOnlyNil(t: AST.TypeInfo): boolean {
	return [...t.possibleTypes].every(
		single => single.type === "SimpleType" && single.value === "nil"
	);
}

function typeListToString(typeList: AST.TypeList): string {
	if (typeList.list.length === 0 && isOnlyNil(typeList.rest)) return "void";
	const all: Array<string> = typeList.list.map(t => typeToString(t));
	if (!isOnlyNil(typeList.rest)) all.push("..." + typeToString(typeList.rest));
	return all.join(", ");
}

function typeListFromType(t: AST.TypeInfo | AST.SingleType): AST.TypeList {
	if (t.type !== "TypeInfo") return typeListFromType(singleToType(t));
	return ast.typeList([t], nil_type);
}

function joinTypeLists(list: Array<AST.TypeList>): AST.TypeList {
	if (list.length === 0) return ast.typeList([], nil_type);
	const last = list.pop();
	const first_types = list.map(tl => getType(tl, 0));
	return ast.typeList([...first_types, ...last.list], last.rest);
}

const literal_map = Object.freeze({
	StringLiteral: string_type,
	NumericLiteral: number_type,
	BooleanLiteral: boolean_type,
	NilLiteral: nil_type,
});

export async function checkFile(
	file: string,
	options?: LuaParseOptions
): Promise<AST.Chunk> {
	const ast = await parseFile(file, options);
	return check(ast);
}

export function checkString(
	code: string,
	options?: LuaParseOptions
): AST.Chunk {
	return check(parse(code, options));
}

export function check(ast_: AST.Chunk): AST.Chunk {
	// This array has the types of local variables in scopes
	const scopes: Array<{ [identifier: string]: ?AST.TypeInfo }> = [];
	// This array has the info for the current function scope
	const function_scopes: Array<{
		return_types: AST.TypeList,
		vararg_types: ?AST.TypeList,
	}> = [];

	function createScope(): void {
		scopes.push({});
	}

	function createFunctionScope(return_types: AST.TypeList): void {
		function_scopes.push({
			return_types,
			vararg_types: null,
		});
	}

	function destroyScope(): void {
		scopes.pop();
	}

	function destroyFunctionScope(): void {
		function_scopes.pop();
	}

	function assignTypeToName(var_: string, type: AST.TypeInfo): void {
		scopes[scopes.length - 1][var_] = type;
	}

	function assignType(var_: AST.Identifier, type: AST.TypeInfo): void {
		return assignTypeToName(var_.name, type);
	}

	function assignVarargsType(types: AST.TypeList): void {
		function_scopes[function_scopes.length - 1].vararg_types = types;
	}

	function getVarargsTypes(): AST.TypeList {
		const types = function_scopes[function_scopes.length - 1].vararg_types;
		if (types == null) throw new Error("No varargs in current context");
		return types;
	}

	function getReturnTypes(): AST.TypeList {
		return function_scopes[function_scopes.length - 1].return_types;
	}

	function getTypeFromScope(name: string): AST.TypeInfo {
		for (let i = scopes.length - 1; i >= 0; i--)
			if (scopes[i][name]) return scopes[i][name];
		return any_type;
	}

	function readLiteral(node: AST.Literal): AST.TypeInfo {
		if (!node.type.endsWith("Literal") || node.type === "VarargLiteral")
			throw new Error("Invalid type");
		return literal_map[node.type];
	}

	function readBinaryExpressionType(
		node: AST.BinaryExpression | AST.LogicalExpression
	): AST.TypeInfo {
		const L: AST.TypeInfo = firstType(readExpression(node.left));
		const R: AST.TypeInfo = firstType(readExpression(node.right));
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
				if (!isNumber(L) || !isNumber(R))
					throw new Error(`Cannot use '${node.operator}' with non-number`);
				return number_type;
			case ">":
			case "<":
			case ">=":
			case "<=":
				if (!isSameSimple(L, R) || (!isNumber(L) && !isString(L)))
					throw new Error(
						`Cannot use '${node.operator}' with non-number or string.`
					);
				return boolean_type;
			case "==":
			case "~=":
				if (!isSameSimple(L, R))
					throw new Error("Cannot compare values of different types");
				return boolean_type;
			case "and":
			case "or":
				// TODO: Actually this should remove some types from L since they
				// should be truthy or falsy
				return ast.typeInfo(new Set([...L.possibleTypes, ...R.possibleTypes]));
			case "..":
				if (!isString(L) || !isString(R))
					throw new Error(`Cannot use '${node.operator}' with non-string`);
				return string_type;
			default:
				throw new Error("Unknown binary operation '" + node.operator + "'");
		}
	}

	function readUnaryExpression(node: AST.UnaryExpression): AST.TypeInfo {
		const type: AST.TypeInfo = firstType(readExpression(node.argument));
		switch (node.operator) {
			case "-":
			case "~":
				if (!isNumber(type))
					throw new Error(`Cannot use '${node.operator}' with non-number.`);
				return number_type;
			case "#":
				if (
					[...type.possibleTypes].some(
						t => t.type !== "TableType" && !isTable(singleToType(t))
					)
				)
					throw new Error(`Cannot use '#' with non-table.`);
				return number_type;
			case "not":
				return boolean_type;
			default:
				throw new Error("Unknown unary operation '" + node.operator + "'");
		}
	}

	function readCallExpressionBase(
		node: AST.Expression | AST.ColonMemberExpression
	): AST.TypeInfo {
		if (node.type === "MemberExpression" && node.indexer === ":") {
			const type: AST.TypeInfo = firstType(readExpression(node.base));
			if (!isTable(type)) throw new Error("Can't index non-table.");
			return function_type;
		} else return firstType(readExpression(node));
	}

	function readCallExpression(
		node:
			| AST.CallExpression
			| AST.StringCallExpression
			| AST.TableCallExpression
	): AST.TypeList {
		const type: AST.TypeInfo = readCallExpressionBase(node.base);
		const arg_types: AST.TypeList = joinTypeLists(
			(node.args: $ReadOnlyArray<AST.Expression>).map(arg =>
				readExpression(arg)
			)
		);
		if (
			[...type.possibleTypes].some(
				t => t.type !== "FunctionType" && !isFunction(singleToType(t))
			)
		)
			throw new Error("Cannot call non-function type.");
		const return_types: Array<AST.TypeList> = [...type.possibleTypes].map(t => {
			if (t.type !== "FunctionType") return ast.typeList([], any_type);
			invariant(t.type === "FunctionType");
			if (!isSupertypeList(t.parameter_types, arg_types))
				throw new Error(
					`Can't call "${singleTypeToString(t)}" with "${typeListToString(
						arg_types
					)}"`
				);
			return t.return_types;
		});
		const n: number = Math.max(...return_types.map(t => t.list.length));
		const list: Array<AST.TypeInfo> = [];
		for (let i = 0; i < n; i++)
			list.push(joinTypes(...return_types.map(t => getType(t, i))));
		const rest: AST.TypeInfo = joinTypes(...return_types.map(t => t.rest));
		return ast.typeList(list, rest);
	}

	function readTableConstructorExpression(
		node: AST.TableConstructorExpression
	): AST.TableType {
		const map: Map<string, AST.TypeInfo> = new Map();
		node.fields.forEach(field => {
			if (field.type === "TableValue") readExpression(field.value);
			else if (field.type === "TableKey") {
				readExpression(field.key);
				readExpression(field.value);
			} else if (field.type === "TableKeyString") {
				const type = firstType(readExpression(field.value));
				map.set(field.key.name, type);
			} else throw new Error("Unknown TableConstructor field");
		});
		return ast.tableType(map);
	}

	function readFunctionNamePrefix(
		node: AST.NonLocalFunctionNamePrefix
	): AST.TypeInfo {
		if (node.type === "Identifier") return getTypeFromScope(node.name);
		else {
			const type: AST.TypeInfo = readFunctionNamePrefix(node.base);
			if (!isTable(type)) throw new Error("Can't index non-table.");
			return any_type;
		}
	}

	function readFunctionName(node: AST.NonLocalFunctionName): AST.TypeInfo {
		if (node.type === "MemberExpression" && node.indexer === ":") {
			const type = readFunctionNamePrefix(node.base);
			if (!isTable(type)) throw new Error("Can't index non-table.");
			return any_type;
		} else return readFunctionNamePrefix(node);
	}

	function readFunctionDeclaration(
		node: AST.FunctionDeclaration
	): AST.FunctionType {
		let self_type: ?AST.TypeInfo;
		const my_type = ast.functionType(
			node.parameter_types,
			node.body.return_types
		);
		if (node.identifier != null) {
			if (node.isLocal)
				assignType(
					(node: AST.LocalNamedFunctionDeclaration).identifier,
					ast.typeInfo(new Set([my_type]))
				);
			else {
				const id = node.identifier;
				assertAssign(readFunctionName(id), function_type);
				if (id.type === "MemberExpression" && id.indexer === ":") {
					self_type = readFunctionNamePrefix(id.base); // TODO duplicate reading, should remove
					if (isAny(self_type)) self_type = table_type;
				}
			}
		}
		createScope();
		createFunctionScope(node.body.return_types);
		if (self_type != null) assignTypeToName("self", self_type);
		for (let i = 0; i < node.parameters.length; i++) {
			const type = getType(node.parameter_types, i);
			assignType(node.parameters[i], type);
		}
		if (node.hasVarargs) {
			const types: Array<AST.TypeInfo> = [];
			if (node.parameter_types != null)
				for (
					let i = node.parameters.length;
					i < node.parameter_types.list.length;
					i++
				)
					types.push(node.parameter_types.list[i]);
			assignVarargsType(ast.typeList(types, node.parameter_types.rest));
		}
		readBlock(node.body);
		destroyScope();
		destroyFunctionScope();
		// Actually if this has an identifier then it is a statement and not
		// an expression, so maybe we should split those cases.
		return my_type;
	}

	function readIdentifier(node: AST.Identifier): AST.TypeInfo {
		return getTypeFromScope(node.name);
	}

	function readIndexExpression(node: AST.IndexExpression): AST.TypeInfo {
		const type: AST.TypeInfo = firstType(readExpression(node.base));
		readExpression(node.index);
		return joinTypes(
			...[...type.possibleTypes].map(t => {
				// TODO: This should be improved
				if (t.type === "TableType") return any_type;
				if (!isTable(singleToType(t)))
					throw new Error("Can't index non-table.");
				return any_type;
			})
		);
	}

	function readMemberExpression(node: AST.MemberExpression): AST.TypeInfo {
		const type: AST.TypeInfo = firstType(readExpression(node.base));
		return joinTypes(
			...[...type.possibleTypes].map(t => {
				if (t.type === "TableType")
					return t.typeMap.get(node.identifier.name) || nil_type;
				if (!isTable(singleToType(t)))
					throw new Error("Can't index non-table.");
				return any_type;
			})
		);
	}

	function readParenthesisExpression(
		node: AST.ParenthesisExpression
	): AST.TypeInfo {
		return firstType(readExpression(node.expression));
	}

	function readExpression(node: AST.Expression): AST.TypeList {
		if (node.type === "VarargLiteral") return getVarargsTypes();
		else if (
			node.type === "StringLiteral" ||
			node.type === "BooleanLiteral" ||
			node.type === "NumericLiteral" ||
			node.type === "NilLiteral"
		)
			return typeListFromType(readLiteral(node));
		else if (node.type === "Identifier")
			return typeListFromType(readIdentifier(node));
		else if (
			node.type === "BinaryExpression" ||
			node.type === "LogicalExpression"
		)
			return typeListFromType(readBinaryExpressionType(node));
		else if (node.type === "UnaryExpression")
			return typeListFromType(readUnaryExpression(node));
		else if (node.type === "CallExpression") return readCallExpression(node);
		else if (node.type === "StringCallExpression")
			return readCallExpression(node);
		else if (node.type === "TableCallExpression")
			return readCallExpression(node);
		else if (node.type === "TableConstructorExpression")
			return typeListFromType(readTableConstructorExpression(node));
		else if (node.type === "FunctionDeclaration")
			return typeListFromType(readFunctionDeclaration(node));
		else if (node.type === "MemberExpression")
			return typeListFromType(readMemberExpression(node));
		else if (node.type === "IndexExpression")
			return typeListFromType(readIndexExpression(node));
		else if (node.type === "ParenthesisExpression")
			return typeListFromType(readParenthesisExpression(node));
		else throw new Error(`Unknown Expression Type '${node.type}'`);
	}

	function readVariable(node: AST.Variable): AST.TypeInfo {
		if (node.type === "Identifier") return readIdentifier(node);
		else if (node.type === "IndexExpression") return readIndexExpression(node);
		else if (node.type === "MemberExpression")
			return readMemberExpression(node);
		else throw new Error(`Unknow Variable Type '${node.type}'`);
	}

	function readLocalStatement(node: AST.LocalStatement): void {
		const init_types = joinTypeLists(
			node.init.map(expr => readExpression(expr))
		);
		const n: number = Math.max(
			init_types.list.length,
			node.typeList.list.length
		);
		for (let i = 0; i < n; i++) {
			const type = getType(node.typeList, i);
			const init_type = getType(init_types, i);
			assertAssign(type, init_type);
		}
		assertAssign(node.typeList.rest, init_types.rest);

		for (let i = 0; i < node.variables.length; i++) {
			const var_ = node.variables[i];
			const type = getType(node.typeList, i);
			assignType(var_, type);
		}
	}

	function readAssignmentStatement(node: AST.AssignmentStatement): void {
		const init_types = joinTypeLists(
			node.init.map(expr => readExpression(expr))
		);
		for (let i = 0; i < node.variables.length; i++) {
			const type = readVariable(node.variables[i]);
			const init_type = getType(init_types, i);
			assertAssign(type, init_type);
		}
	}

	function readCallStatement(node: AST.CallStatement): void {
		readCallExpression(node.expression);
	}

	function readWhileStatement(node: AST.WhileStatement): void {
		const type: AST.TypeInfo = firstType(readExpression(node.condition));
		if (!isBoolean(type))
			throw new Error("While condition can't be non-boolean.");
		createScope();
		readBlock(node.body);
		destroyScope();
	}

	function readRepeatStatement(node: AST.RepeatStatement): void {
		createScope();
		readBlock(node.body);
		const type: AST.TypeInfo = firstType(readExpression(node.condition));
		if (!isBoolean(type))
			throw new Error("Repeat condition can't be non-boolean.");
		destroyScope();
	}

	// eslint-disable-next-line no-unused-vars
	function readGotoStatement(node: AST.GotoStatement): void {}

	// eslint-disable-next-line no-unused-vars
	function readLabelStatement(node: AST.LabelStatement): void {}

	function readReturnStatement(node: AST.ReturnStatement): void {
		const types: AST.TypeList = joinTypeLists(
			node.args.map(arg => readExpression(arg))
		);
		const return_types: AST.TypeList = getReturnTypes();
		if (return_types && !isSupertypeList(return_types, types))
			throw new Error(
				`Return type is "${typeListToString(
					types
				)}" and should be "${typeListToString(return_types)}"`
			);
	}

	function readIfStatement(node: AST.IfStatement): void {
		node.clauses.forEach(clause => {
			if (clause.type !== "ElseClause") {
				const type: AST.TypeInfo = firstType(readExpression(clause.condition));
				if (!isBoolean(type))
					throw new Error("If condition can't be non-boolean.");
			}
			createScope();
			readBlock(clause.body);
			destroyScope();
		});
	}

	function readDoStatement(node: AST.DoStatement): void {
		createScope();
		readBlock(node.body);
		destroyScope();
	}

	// eslint-disable-next-line no-unused-vars
	function readBreakStatement(node: AST.BreakStatement): void {}

	function readForNumericStatement(node: AST.ForNumericStatement): void {
		const start_type: AST.TypeInfo = firstType(readExpression(node.start));
		const end_type: AST.TypeInfo = firstType(readExpression(node.end));
		const step_type: AST.TypeInfo =
			node.step != null ? firstType(readExpression(node.step)) : any_type;
		if (!isNumber(start_type) || !isNumber(end_type) || !isNumber(step_type))
			throw new Error("NumericFor limits should be integers");
		createScope();
		assignType(node.variable, number_type);
		readBlock(node.body);
		destroyScope();
	}

	function readForGenericStatement(node: AST.ForGenericStatement): void {
		// TODO: deal properly with types here
		node.iterators.forEach(it => readExpression(it));
		createScope();
		node.variables.forEach(var_ => assignType(var_, any_type));
		readBlock(node.body);
		destroyScope();
	}

	function readStatement(node: AST.Statement): void {
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

	function readBlock(block: AST.SimpleBlock | AST.FunctionBlock): void {
		block.statements.forEach(node => readStatement(node));
	}

	function readChunk(node: AST.Chunk): void {
		createFunctionScope(node.body.return_types);
		createScope();
		assignVarargsType(ast.typeList([], nil_type));
		readBlock(node.body);
		destroyScope();
		destroyFunctionScope();
	}

	readChunk(ast_);
	return ast_;
}
