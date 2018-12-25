// @flow strict
import * as AST from "./ast-types";

import invariant from "assert";

type enterExit<T> = {
	enter?: T,
	exit?: T,
};

type expressionFunc<T> = T => ?AST.Expression;
type expressionFuncs<T> = enterExit<expressionFunc<T>>;

type statementFunc<T> = T => ?AST.Statement;
type statementFuncs<T> = enterExit<statementFunc<T>>;

export interface Visitor {
	// Expression
	VarargLiteral?: expressionFuncs<AST.VarargLiteral>;
	StringLiteral?: expressionFuncs<AST.StringLiteral>;
	BooleanLiteral?: expressionFuncs<AST.BooleanLiteral>;
	NumericLiteral?: expressionFuncs<AST.NumericLiteral>;
	NilLiteral?: expressionFuncs<AST.NilLiteral>;
	Identifier?: expressionFuncs<AST.Identifier>;
	BinaryExpression?: expressionFuncs<AST.BinaryExpression>;
	LogicalExpression?: expressionFuncs<AST.LogicalExpression>;
	UnaryExpression?: expressionFuncs<AST.UnaryExpression>;
	CallExpression?: expressionFuncs<AST.CallExpression>;
	StringCallExpression?: expressionFuncs<AST.StringCallExpression>;
	TableCallExpression?: expressionFuncs<AST.TableCallExpression>;
	TableConstructorExpression?: expressionFuncs<AST.TableConstructorExpression>;
	FunctionExpression?: expressionFuncs<AST.FunctionExpression>;
	MemberExpression?: expressionFuncs<AST.MemberExpression>;
	IndexExpression?: expressionFuncs<AST.IndexExpression>;
	ParenthesisExpression?: expressionFuncs<AST.ParenthesisExpression>;

	// Statement
	LocalStatement?: statementFuncs<AST.LocalStatement>;
	AssignmentStatement?: statementFuncs<AST.AssignmentStatement>;
	CallStatement?: statementFuncs<AST.CallStatement>;
	WhileStatement?: statementFuncs<AST.WhileStatement>;
	RepeatStatement?: statementFuncs<AST.RepeatStatement>;
	LocalFunctionStatement?: statementFuncs<AST.LocalFunctionStatement>;
	NonLocalFunctionStatement?: statementFuncs<AST.NonLocalFunctionStatement>;
	GotoStatement?: statementFuncs<AST.GotoStatement>;
	LabelStatement?: statementFuncs<AST.LabelStatement>;
	BreakStatement?: statementFuncs<AST.BreakStatement>;
	DeclareStatement?: statementFuncs<AST.DeclareStatement>;
	ReturnStatement?: statementFuncs<AST.ReturnStatement>;
	IfStatement?: statementFuncs<AST.IfStatement>;
	DoStatement?: statementFuncs<AST.DoStatement>;
	ForNumericStatement?: statementFuncs<AST.ForNumericStatement>;
	ForGenericStatement?: statementFuncs<AST.ForGenericStatement>;

	// TableConstructor stuff
	TableKey?: enterExit<
		(AST.TableKey) => ?(AST.TableKey | AST.TableValue | AST.TableKeyString)
	>;
	TableValue?: enterExit<
		(AST.TableValue) => ?(AST.TableKey | AST.TableValue | AST.TableKeyString)
	>;
	TableKeyString?: enterExit<
		(
			AST.TableKeyString
		) => ?(AST.TableKey | AST.TableValue | AST.TableKeyString)
	>;

	// Block
	SimpleBlock?: enterExit<(AST.SimpleBlock) => ?AST.SimpleBlock>;
	FunctionBlock?: enterExit<(AST.SimpleBlock) => ?AST.FunctionBlock>;
	Chunk?: enterExit<(AST.Chunk) => ?AST.Chunk>;

	// Other
	ColonMemberExpression?: enterExit<
		(AST.ColonMemberExpression) => ?(AST.Expression | AST.ColonMemberExpression)
	>;

	// Scope
	+createScope?: () => mixed;
	+destroyScope?: () => mixed;
}

function callVisitors<T>(
	node: T,
	visitors: Array<Visitor>,
	type: "enter" | "exit"
): T {
	return visitors.reduce((node: T, visitor: Visitor) => {
		// $FlowFixMe how to type this correctly?
		const f: ?(T) => ?T = visitor[node.type] && visitor[node.type][type];
		if (f != null) {
			const n = f(node);
			if (n != null) return n;
		}
		return node;
	}, node);
}

export function visit(ast_: AST.Chunk, visitors: Array<Visitor>): AST.Chunk {
	function createScope(): void {
		visitors.forEach(v => v.createScope && v.createScope());
	}

	function destroyScope(): void {
		visitors.forEach(v => v.destroyScope && v.destroyScope());
	}

	function readCallExpressionBase(
		node_: AST.Expression | AST.ColonMemberExpression
	): AST.Expression | AST.ColonMemberExpression {
		const node = callVisitors<AST.Expression | AST.ColonMemberExpression>(
			node_,
			visitors,
			"enter"
		);
		if (node.type === "MemberExpression" && node.indexer === ":") {
			node.base = readExpression(node.base);
			return callVisitors<AST.Expression | AST.ColonMemberExpression>(
				node,
				visitors,
				"exit"
			);
		} else return readExpression(node, false);
	}

	function readTableConstructorExpression(
		node: AST.TableConstructorExpression
	): void {
		node.fields = node.fields.map(field_ => {
			const field = callVisitors<
				AST.TableKey | AST.TableValue | AST.TableKeyString
			>(field_, visitors, "enter");
			if (field.type === "TableKey") field.key = readExpression(field.key);
			field.value = readExpression(field.value);
			return callVisitors<AST.TableKey | AST.TableValue | AST.TableKeyString>(
				field,
				visitors,
				"exit"
			);
		});
	}

	function readFunctionBase(node: { ...AST.FunctionBase }): void {
		createScope();
		node.parameters = node.parameters.map(p_ => {
			const p = readExpression(p_);
			if (p.type !== "Identifier")
				throw new Error(
					"INTERNAL ERROR: Can't replace parameter node with non-identifier."
				);
			return (p: AST.Identifier);
		});
		node.body = readBlock(node.body);
		destroyScope();
	}

	function readExpression(
		node_: AST.Expression,
		visit: boolean = true
	): AST.Expression {
		const node = visit
			? callVisitors<AST.Expression>(node_, visitors, "enter")
			: node_;

		switch (node.type) {
			case "VarargLiteral":
			case "StringLiteral":
			case "BooleanLiteral":
			case "NumericLiteral":
			case "NilLiteral":
			case "Identifier":
				// Terminals
				break;
			case "BinaryExpression":
			case "LogicalExpression":
				node.left = readExpression(node.left);
				node.right = readExpression(node.right);
				break;
			case "UnaryExpression":
				node.argument = readExpression(node.argument);
				break;
			case "StringCallExpression": {
				node.base = readCallExpressionBase(node.base);
				const expr = readExpression(node.args[0]);
				if (expr.type !== "StringLiteral")
					throw new Error(
						"PLUGIN ERROR: Can't replace with non-StringLiteral on StringCallExpression"
					);
				if (expr !== node.args[0]) node.args = [expr];
				break;
			}
			case "TableCallExpression": {
				node.base = readCallExpressionBase(node.base);
				const expr = readExpression(node.args[0]);
				if (expr.type !== "TableConstructorExpression")
					throw new Error(
						"PLUGIN ERROR: Can't replace with non-TableConstructorExpression on TableCallExpression"
					);
				if (expr !== node.args[0]) node.args = [expr];
				break;
			}
			case "CallExpression":
				node.base = readCallExpressionBase(node.base);
				node.args = (node.args: $ReadOnlyArray<AST.Expression>).map(a =>
					readExpression(a)
				);
				break;
			case "TableConstructorExpression":
				readTableConstructorExpression(node);
				break;
			case "FunctionExpression":
				readFunctionBase(node);
				break;
			case "MemberExpression":
				node.base = readExpression(node.base);
				const id = readExpression(node.identifier);
				if (id.type !== "Identifier")
					throw new Error(
						"PLUGIN ERROR: Can't replace identifier with non-Identifier in MemberExpression"
					);
				node.identifier = id;
				break;
			case "IndexExpression":
				node.base = readExpression(node.base);
				node.index = readExpression(node.index);
				break;
			case "ParenthesisExpression":
				node.expression = readExpression(node.expression);
				break;
			default:
				throw new Error(`Unknown Expression Type '${node.type}'`);
		}
		return callVisitors<AST.Expression>(node, visitors, "exit");
	}

	function readIfStatement(node: AST.IfStatement): void {
		node.clauses.forEach(clause => {
			if (clause.type !== "ElseClause")
				clause.condition = readExpression(clause.condition);
			createScope();
			clause.body = readBlock(clause.body);
			destroyScope();
		});
	}

	function readForNumericStatement(node: AST.ForNumericStatement): void {
		node.start = readExpression(node.start);
		node.end = readExpression(node.end);
		if (node.step != null) node.step = readExpression(node.step);
		createScope();
		node.body = readBlock(node.body);
		destroyScope();
	}

	function readStatement(node_: AST.Statement): AST.Statement {
		const node = callVisitors<AST.Statement>(node_, visitors, "enter");

		switch (node.type) {
			case "LocalStatement":
				node.variables = (node.variables: $ReadOnlyArray<AST.Expression>).map(
					v_ => {
						const v = readExpression(v_);
						if (v.type !== "Identifier")
							throw new Error(
								"PLUGIN ERROR: Can't replace LocalStatement's variable with non-Identifier"
							);
						return v;
					}
				);
				node.init = node.init.map(e => readExpression(e));
				break;
			case "AssignmentStatement":
				node.variables = (node.variables: $ReadOnlyArray<AST.Expression>).map(
					v_ => {
						const v = readExpression(v_);
						if (
							!(
								v.type === "Identifier" ||
								v.type === "IndexExpression" ||
								(v.type === "MemberExpression" && v.indexer === ".")
							)
						)
							throw new Error(
								"PLUGIN ERROR: Can't replace AssignmentStatement's variable with non-Variable"
							);
						return v;
					}
				);
				node.init = node.init.map(e => readExpression(e));
				break;
			case "CallStatement": {
				const expr = readExpression(node.expression);
				if (
					expr.type !== "CallExpression" &&
					expr.type !== "TableCallExpression" &&
					expr.type !== "StringCallExpression"
				)
					throw new Error(
						"PLUGIN ERROR: Can't replace CallStatement's expression with non-CallExpression-like"
					);
				node.expression = expr;
				break;
			}
			case "WhileStatement":
				node.condition = readExpression(node.condition);
				createScope();
				node.body = readBlock(node.body);
				destroyScope();
				break;
			case "RepeatStatement":
				createScope();
				node.body = readBlock(node.body);
				node.condition = readExpression(node.condition);
				destroyScope();
				break;
			case "LocalFunctionStatement":
			case "NonLocalFunctionStatement":
				readFunctionBase(node);
				break;
			case "GotoStatement":
			case "LabelStatement":
			case "BreakStatement":
			case "DeclareStatement":
				// terminal
				break;
			case "ReturnStatement":
				node.args = node.args.map(a => readExpression(a));
				break;
			case "IfStatement":
				readIfStatement(node);
				break;
			case "DoStatement":
				createScope();
				node.body = readBlock(node.body);
				destroyScope();
				break;
			case "ForNumericStatement":
				readForNumericStatement(node);
				break;
			case "ForGenericStatement":
				node.iterators = node.iterators.map(i => readExpression(i));
				createScope();
				node.variables = node.variables.map(v_ => {
					const v = readExpression(v_);
					if (v.type !== "Identifier")
						throw new Error(
							"PLUGIN ERROR: Can't replace Generic For variable with non-Identifier"
						);
					return v;
				});
				node.body = readBlock(node.body);
				destroyScope();
				break;
			default:
				throw new Error(`Unknown Statement Type '${node.type}'`);
		}
		return callVisitors<AST.Statement>(node, visitors, "exit");
	}

	function readBlock<Block: { statements: $ReadOnlyArray<AST.Statement> }>(
		block_: Block
	): Block {
		const block = callVisitors<Block>(block_, visitors, "enter");
		block.statements = block.statements.map(s => readStatement(s));
		return callVisitors<Block>(block, visitors, "exit");
	}

	function readChunk(node_: AST.Chunk): AST.Chunk {
		const node = callVisitors<AST.Chunk>(node_, visitors, "enter");
		createScope();
		node.body = readBlock(node.body);
		destroyScope();
		return callVisitors<AST.Chunk>(node, visitors, "exit");
	}

	return readChunk(ast_);
}
