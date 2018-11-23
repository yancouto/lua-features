// @flow
import { ast, parse } from "./lua-parse";

import type {
	LuaParseOptions,
	NodeAssignmentStatement,
	NodeBinaryExpression,
	NodeBreakStatement,
	NodeCallExpression,
	NodeCallStatement,
	NodeChunk,
	NodeColonMemberExpression,
	NodeDoStatement,
	NodeExpression,
	NodeForGenericStatement,
	NodeForNumericStatement,
	NodeFunctionDeclaration,
	NodeGotoStatement,
	NodeIdentifier,
	NodeIfStatement,
	NodeIndexExpression,
	NodeLabelStatement,
	NodeLiteral,
	NodeLocalNamedFunctionDeclaration,
	NodeLocalStatement,
	NodeLogicalExpression,
	NodeMemberExpression,
	NodeNonLocalFunctionName,
	NodeNonLocalFunctionNamePrefix,
	NodeParenthesisExpression,
	NodeRepeatStatement,
	NodeReturnStatement,
	NodeSimpleType,
	NodeStatement,
	NodeStringCallExpression,
	NodeTableCallExpression,
	NodeTableConstructorExpression,
	NodeTypeInfo,
	NodeTypeList,
	NodeUnaryExpression,
	NodeVarargLiteral,
	NodeVariable,
	NodeWhileStatement,
} from "./lua-parse";

type TypeInfo = $ReadOnly<NodeTypeInfo>;

const nil_type: NodeSimpleType = Object.freeze(ast.simpleType("nil"));
const any_type: NodeSimpleType = Object.freeze(ast.simpleType("any"));
const number_type: NodeSimpleType = Object.freeze(ast.simpleType("number"));
const string_type: NodeSimpleType = Object.freeze(ast.simpleType("string"));
const boolean_type: NodeSimpleType = Object.freeze(ast.simpleType("boolean"));
const table_type: NodeSimpleType = Object.freeze(ast.simpleType("table"));
const function_type: NodeSimpleType = Object.freeze(ast.simpleType("function"));

function isSupertype(sup: TypeInfo, sub: TypeInfo): boolean {
	if (sup.type === "SimpleType" && sup.value === "any") return true;
	// should this be allowed?
	if (sub.type === "SimpleType" && sub.value === "any") return true;
	if (sub.type === "SimpleType" && sup.type === "SimpleType")
		return sub.value === sup.value;
	if (isFunction(sup) && sub.type === "FunctionType") return true;
	if (sub.type === "FunctionType" && sup.type === "FunctionType")
		return (
			isSupertypeList(sup.parameter_types, sub.parameter_types) &&
			isSupertypeList(sup.return_types, sub.return_types)
		);
	return false;
}

function isSupertypeList(sup: NodeTypeList, sub: NodeTypeList): boolean {
	const n = Math.max(sup.list.length, sub.list.length);
	for (let i = 0; i < n; i++) {
		const sup_type = getType(sup, i);
		const sub_type = getType(sub, i);
		if (!isSupertype(sup_type, sub_type)) return false;
	}
	return isSupertype(sup.rest, sub.rest);
}

function assertAssign(a: TypeInfo, b: TypeInfo): void {
	if (!isSupertype(a, b))
		throw new Error(
			`Can't assign "${typeToString(b)}" to "${typeToString(a)}".`
		);
}

function isSameSimple(a: TypeInfo, b: TypeInfo): boolean {
	return (
		a.type === "SimpleType" &&
		b.type === "SimpleType" &&
		(a.value === "any" || b.value === "any" || a.value === b.value)
	);
}

function isAny(t: TypeInfo): boolean {
	return t.type === "SimpleType" && t.value === "any";
}

function isNumber(t: TypeInfo): boolean {
	return isSameSimple(t, number_type);
}

function isString(t: TypeInfo): boolean {
	return isSameSimple(t, string_type);
}

function isBoolean(t: TypeInfo): boolean {
	return isSameSimple(t, boolean_type);
}

function isTable(t: TypeInfo): boolean {
	return isSameSimple(t, table_type);
}

function isFunction(t: TypeInfo): boolean {
	return isSameSimple(t, function_type);
}

function getType(tl: NodeTypeList, i: number): NodeTypeInfo {
	if (i < tl.list.length) return tl.list[i];
	else return tl.rest;
}

function firstType(tl: NodeTypeList): NodeTypeInfo {
	return getType(tl, 0);
}

function typeToString(t: TypeInfo): string {
	if (t.type === "SimpleType") return t.value;
	if (t.type === "FunctionType")
		return `(${typeListToString(t.parameter_types)}) => (${typeListToString(
			t.return_types
		)})`;
	throw new Error(`Unknow TypeInfo type '${t.type}'`);
}

function isOnlyNil(t: TypeInfo): boolean {
	return t.type === "SimpleType" && t.value === "nil";
}

function typeListToString(typeList: NodeTypeList): string {
	if (typeList.list.length === 0 && isOnlyNil(typeList.rest)) return "void";
	const all: Array<string> = typeList.list.map(t => typeToString(t));
	if (!isOnlyNil(typeList.rest)) all.push("..." + typeToString(typeList.rest));
	return all.join(", ");
}

function typeListFromTypeInfo(t: NodeTypeInfo): NodeTypeList {
	return ast.typeList([t], nil_type);
}

function joinTypeLists(list: Array<NodeTypeList>): NodeTypeList {
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

export function check(
	code: string,
	options: LuaParseOptions = Object.freeze({})
): NodeChunk {
	// This array has the types of local variables in scopes
	const scopes: Array<{ [identifier: string]: ?TypeInfo }> = [];
	// This array has the info for the current function scope
	const function_scopes: Array<{
		return_types: NodeTypeList,
		vararg_types: ?NodeTypeList,
	}> = [];

	function createScope(): void {
		scopes.push({});
	}

	function createFunctionScope(return_types: NodeTypeList): void {
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

	function assignTypeToName(var_: string, type: TypeInfo): void {
		scopes[scopes.length - 1][var_] = type;
	}

	function assignType(var_: NodeIdentifier, type: TypeInfo): void {
		return assignTypeToName(var_.name, type);
	}

	function assignVarargsType(types: NodeTypeList): void {
		function_scopes[function_scopes.length - 1].vararg_types = types;
	}

	function getVarargsTypes(): NodeTypeList {
		const types = function_scopes[function_scopes.length - 1].vararg_types;
		if (types == null) throw new Error("No varargs in current context");
		return types;
	}

	function getReturnTypes(): NodeTypeList {
		return function_scopes[function_scopes.length - 1].return_types;
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

	// eslint-disable-next-line no-unused-vars
	function readVarargLiteralSingle(node: NodeVarargLiteral): TypeInfo {
		return firstType(getVarargsTypes());
	}

	function readBinaryExpressionType(
		node: NodeBinaryExpression | NodeLogicalExpression
	): TypeInfo {
		const L: TypeInfo = firstType(readExpression(node.left));
		const R: TypeInfo = firstType(readExpression(node.right));
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
				// TODO: This should be the union of the types
				return any_type;
			case "..":
				if (!isString(L) || !isString(R))
					throw new Error(`Cannot use '${node.operator}' with non-string`);
				return string_type;
			default:
				throw new Error("Unknown binary operation '" + node.operator + "'");
		}
	}

	function readUnaryExpression(node: NodeUnaryExpression): TypeInfo {
		const type: TypeInfo = firstType(readExpression(node.argument));
		switch (node.operator) {
			case "-":
			case "~":
				if (!isNumber(type))
					throw new Error(`Cannot use '${node.operator}' with non-number.`);
				return number_type;
			case "#":
				if (!isTable(type)) throw new Error(`Cannot use '#' with non-table.`);
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
			const type: TypeInfo = firstType(readExpression(node.base));
			if (!isTable(type)) throw new Error("Can't index non-table.");
			return function_type;
		} else return firstType(readExpression(node));
	}

	function readCallExpression(
		node:
			| NodeCallExpression
			| NodeStringCallExpression
			| NodeTableCallExpression
	): NodeTypeList {
		const type: TypeInfo = readCallExpressionBase(node.base);
		const argument_types = joinTypeLists(
			(node.arguments: $ReadOnlyArray<NodeExpression>).map(arg =>
				readExpression(arg)
			)
		);
		if (isFunction(type)) return ast.typeList([], any_type);
		if (type.type !== "FunctionType")
			throw new Error("Cannot call non-function type.");
		if (!isSupertypeList(type.parameter_types, argument_types))
			throw new Error(
				`Can't call function that accepts "${typeListToString(
					type.parameter_types
				)}" with "${typeListToString(argument_types)}".`
			);
		return type.return_types;
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
			const type: TypeInfo = readFunctionNamePrefix(node.base);
			if (!isTable(type)) throw new Error("Can't index non-table.");
			return any_type;
		}
	}

	function readFunctionName(node: NodeNonLocalFunctionName): TypeInfo {
		if (node.type === "MemberExpression" && node.indexer === ":") {
			const type = readFunctionNamePrefix(node.base);
			if (!isTable(type)) throw new Error("Can't index non-table.");
			return any_type;
		} else return readFunctionNamePrefix(node);
	}

	function readFunctionDeclaration(node: NodeFunctionDeclaration): TypeInfo {
		let self_type: ?TypeInfo;
		const my_type = ast.functionType(node.parameter_types, node.return_types);
		if (node.identifier != null) {
			if (node.isLocal)
				assignType(
					(node: NodeLocalNamedFunctionDeclaration).identifier,
					my_type
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
		createFunctionScope(node.return_types);
		if (self_type != null) assignTypeToName("self", self_type);
		for (let i = 0; i < node.parameters.length; i++) {
			const type = getType(node.parameter_types, i);
			assignType(node.parameters[i], type);
		}
		if (node.hasVarargs) {
			const types: Array<NodeTypeInfo> = [];
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

	function readIdentifier(node: NodeIdentifier): TypeInfo {
		return getTypeFromScope(node.name);
	}

	function readIndexExpression(node: NodeIndexExpression): TypeInfo {
		const type: TypeInfo = firstType(readExpression(node.base));
		if (!isTable(type)) throw new Error("Can't index non-table.");
		readExpression(node.index);
		return any_type;
	}

	function readMemberExpression(node: NodeMemberExpression): TypeInfo {
		const type: TypeInfo = firstType(readExpression(node.base));
		if (!isTable(type)) throw new Error("Can't index non-table.");
		return any_type;
	}

	function readParenthesisExpression(
		node: NodeParenthesisExpression
	): TypeInfo {
		return firstType(readExpression(node.expression));
	}

	function readExpression(node: NodeExpression): NodeTypeList {
		if (node.type === "VarargLiteral") return getVarargsTypes();
		else if (
			node.type === "StringLiteral" ||
			node.type === "BooleanLiteral" ||
			node.type === "NumericLiteral" ||
			node.type === "NilLiteral"
		)
			return typeListFromTypeInfo(readLiteral(node));
		else if (node.type === "Identifier")
			return typeListFromTypeInfo(readIdentifier(node));
		else if (
			node.type === "BinaryExpression" ||
			node.type === "LogicalExpression"
		)
			return typeListFromTypeInfo(readBinaryExpressionType(node));
		else if (node.type === "UnaryExpression")
			return typeListFromTypeInfo(readUnaryExpression(node));
		else if (node.type === "CallExpression") return readCallExpression(node);
		else if (node.type === "StringCallExpression")
			return readCallExpression(node);
		else if (node.type === "TableCallExpression")
			return readCallExpression(node);
		else if (node.type === "TableConstructorExpression")
			return typeListFromTypeInfo(readTableConstructorExpression(node));
		else if (node.type === "FunctionDeclaration")
			return typeListFromTypeInfo(readFunctionDeclaration(node));
		else if (node.type === "MemberExpression")
			return typeListFromTypeInfo(readMemberExpression(node));
		else if (node.type === "IndexExpression")
			return typeListFromTypeInfo(readIndexExpression(node));
		else if (node.type === "ParenthesisExpression")
			return typeListFromTypeInfo(readParenthesisExpression(node));
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

	function readAssignmentStatement(node: NodeAssignmentStatement): void {
		const init_types = joinTypeLists(
			node.init.map(expr => readExpression(expr))
		);
		for (let i = 0; i < node.variables.length; i++) {
			const type = readVariable(node.variables[i]);
			const init_type = getType(init_types, i);
			assertAssign(type, init_type);
		}
	}

	function readCallStatement(node: NodeCallStatement): void {
		readCallExpression(node.expression);
	}

	function readWhileStatement(node: NodeWhileStatement): void {
		const type: TypeInfo = firstType(readExpression(node.condition));
		if (!isBoolean(type))
			throw new Error("While condition can't be non-boolean.");
		createScope();
		readBlock(node.body);
		destroyScope();
	}

	function readRepeatStatement(node: NodeRepeatStatement): void {
		createScope();
		readBlock(node.body);
		const type: TypeInfo = firstType(readExpression(node.condition));
		if (!isBoolean(type))
			throw new Error("Repeat condition can't be non-boolean.");
		destroyScope();
	}

	// eslint-disable-next-line no-unused-vars
	function readGotoStatement(node: NodeGotoStatement): void {}

	// eslint-disable-next-line no-unused-vars
	function readLabelStatement(node: NodeLabelStatement): void {}

	function readReturnStatement(node: NodeReturnStatement): void {
		const types: NodeTypeList = joinTypeLists(
			node.arguments.map(arg => readExpression(arg))
		);
		const return_types: NodeTypeList = getReturnTypes();
		if (return_types && !isSupertypeList(return_types, types))
			throw new Error(
				`Return type is "${typeListToString(
					types
				)}" and should be "${typeListToString(return_types)}"`
			);
	}

	function readIfStatement(node: NodeIfStatement): void {
		node.clauses.forEach(clause => {
			if (clause.type !== "ElseClause") {
				const type: TypeInfo = firstType(readExpression(clause.condition));
				if (!isBoolean(type))
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

	// eslint-disable-next-line no-unused-vars
	function readBreakStatement(node: NodeBreakStatement): void {}

	function readForNumericStatement(node: NodeForNumericStatement): void {
		const start_type: TypeInfo = firstType(readExpression(node.start));
		const end_type: TypeInfo = firstType(readExpression(node.end));
		const step_type: TypeInfo =
			node.step != null ? firstType(readExpression(node.step)) : any_type;
		if (!isNumber(start_type) || !isNumber(end_type) || !isNumber(step_type))
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
		createFunctionScope(ast.typeList([], nil_type));
		createScope();
		assignVarargsType(ast.typeList([], nil_type));
		readBlock(node.body);
		destroyScope();
		destroyFunctionScope();
	}

	const ast_ = parse(code, options);
	readChunk(ast_);
	return ast_;
}
