// @flow strict
/* eslint-disable no-unused-vars */
// Many vars are unused in this file

import * as AST from "./ast-types";

export type GenerateOptions = {||};

const default_options = {};

export function generate(ast: AST.Chunk, _options?: GenerateOptions): string {
	const options = { ...default_options, ..._options };
	let curLine: number = 1;
	const buffer: Array<string> = [];

	function adjust(pos: ?AST.Position, level: number): void {
		if (pos == null || curLine >= pos.line) {
			buffer.push(" ");
		} else {
			buffer.push("\n".repeat(pos.line - curLine));
			buffer.push("\t".repeat(level));
			curLine = pos.line;
		}
	}

	function start(node: {
		loc?: {| start: AST.Position, end: AST.Position |},
	}): ?AST.Position {
		return node.loc && node.loc.start;
	}

	function end(node: {
		loc?: {| start: AST.Position, end: AST.Position |},
	}): ?AST.Position {
		return node.loc && node.loc.end;
	}

	function genChunk(node: AST.Chunk): void {
		return genBlock(node.body, 0);
	}

	function genBlock(
		body: AST.SimpleBlock | AST.FunctionBlock,
		level: number
	): void {
		body.statements.forEach(node => genStatement(node, level));
	}

	function genStatement(node: AST.Statement, level: number): void {
		adjust(start(node), level);
		switch (node.type) {
			case "DeclareStatement":
				// no output
				return;
			case "LocalStatement":
				return genAssignmentStatement(node, level);
			case "CallStatement":
				return genCallStatement(node, level);
			case "WhileStatement":
				return genWhileStatement(node, level);
			case "RepeatStatement":
				return genRepeatStatement(node, level);
			case "AssignmentStatement":
				return genAssignmentStatement(node, level);
			case "FunctionDeclaration":
				return genFunctionDeclaration(node, level);
			case "GotoStatement":
				return genGotoStatement(node, level);
			case "LabelStatement":
				return genLabelStatement(node, level);
			case "ReturnStatement":
				return genReturnStatement(node, level);
			case "IfStatement":
				return genIfStatement(node, level);
			case "DoStatement":
				return genDoStatement(node, level);
			case "BreakStatement":
				return genBreakStatement(node, level);
			case "ForNumericStatement":
				return genForNumericStatement(node, level);
			case "ForGenericStatement":
				return genForGenericStatement(node, level);
			default:
				throw new Error(`Unknow Statement type "${node.type}"`);
		}
	}

	function genBreakStatement(node: AST.BreakStatement, level: number): void {
		buffer.push("break");
	}

	function genDoStatement(node: AST.DoStatement, level: number): void {
		buffer.push("do");
		genBlock(node.body, level + 1);
		adjust(end(node), level);
		buffer.push("end");
	}

	function genLabelStatement(node: AST.LabelStatement, level: number): void {
		buffer.push(`::${node.label.name}::`);
	}

	function genGotoStatement(node: AST.GotoStatement, level: number): void {
		buffer.push(`goto ${node.label.name}`);
	}

	function genCallStatement(node: AST.CallStatement, level: number): void {
		genExpression(node.expression, level);
	}

	function genWhileStatement(node: AST.WhileStatement, level: number): void {
		buffer.push("while");
		genExpression(node.condition, level);
		buffer.push(" do");
		genBlock(node.body, level + 1);
		adjust(end(node), level);
		buffer.push("end");
	}

	function genRepeatStatement(node: AST.RepeatStatement, level: number): void {
		buffer.push("repeat");
		genBlock(node.body, level + 1);
		buffer.push(" until");
		genExpression(node.condition, level);
	}

	function genReturnStatement(node: AST.ReturnStatement, level: number): void {
		buffer.push("return");
		node.args.forEach(arg => {
			genExpression(arg, level);
			buffer.push(", ");
		});
		if (node.args.length > 0) buffer.pop();
	}

	function genIfClause(
		c: AST.IfClause | AST.ElseifClause | AST.ElseClause,
		level: number
	): void {
		adjust(start(c), level);
		if (c.type === "ElseClause") buffer.push("else");
		else {
			buffer.push(c.type === "IfClause" ? "if" : "elseif");
			genExpression(c.condition, level);
			buffer.push(" then");
		}
		genBlock(c.body, level + 1);
	}

	function genIfStatement(node: AST.IfStatement, level: number): void {
		node.clauses.forEach(c => genIfClause(c, level));
		adjust(end(node), level);
		buffer.push("end");
	}

	function genAssignmentStatement(
		node: AST.AssignmentStatement | AST.LocalStatement,
		level: number
	): void {
		if (node.type === "LocalStatement") buffer.push("local");
		const vs: $ReadOnlyArray<AST.Variable> = node.variables;
		vs.forEach(v => {
			genExpression(v, level);
			buffer.push(", ");
		});
		if (node.variables.length > 0) buffer.pop();
		if (node.init.length > 0) {
			buffer.push(" =");
			node.init.forEach(exp => {
				genExpression(exp, level);
				buffer.push(", ");
			});
			buffer.pop();
		}
	}

	function genForNumericStatement(
		node: AST.ForNumericStatement,
		level: number
	): void {
		buffer.push("for");
		genIdentifier(node.variable, level);
		buffer.push(" =");
		genExpression(node.start, level + 1);
		buffer.push(",");
		genExpression(node.end, level + 1);
		if (node.step != null) {
			const s = node.step;
			buffer.push(",");
			genExpression(s, level + 1);
		}
		buffer.push(" do");
		genBlock(node.body, level + 1);
		adjust(end(node), level);
		buffer.push("end");
	}

	function genForGenericStatement(
		node: AST.ForGenericStatement,
		level: number
	): void {
		buffer.push("for ");
		node.variables.forEach(v => {
			genIdentifier(v, level);
			buffer.push(", ");
		});
		buffer.pop();
		buffer.push(" in");
		node.iterators.forEach(it => {
			genExpression(it, level);
			buffer.push(", ");
		});
		buffer.pop();
		buffer.push(" do");
		adjust(end(node), level);
		buffer.push("end");
	}

	function genFunctionBase(node: AST.FunctionDeclaration, level: number): void {
		buffer.push("(");
		node.parameters.forEach(p => {
			genIdentifier(p, level);
			buffer.push(", ");
		});
		if (node.parameters.length > 0 && !node.hasVarargs) buffer.pop();
		if (node.hasVarargs) buffer.push(" ...");
		buffer.push(")");

		genBlock(node.body, level + 1);
		adjust(end(node), level);
		buffer.push("end");
	}

	function genFunctionName(
		node: AST.NonLocalFunctionName | AST.NonLocalFunctionNamePrefix,
		level: number
	): void {
		if (node.type === "Identifier") genIdentifier(node, level);
		else {
			genFunctionName(node.base, level);
			buffer.push(node.indexer);
			genIdentifier(node.identifier, level);
		}
	}

	function genFunctionDeclaration(
		node: AST.FunctionDeclaration,
		level: number
	): void {
		if (node.isLocal) {
			// Local Named
			buffer.push("local function ");
			genIdentifier(node.identifier, level);
			genFunctionBase(node, level);
		} else if (node.identifier != null) {
			// NonLocal Named
			const id = node.identifier;
			buffer.push("function");
			genFunctionName(id, level);
			genFunctionBase(node, level);
		} else {
			// Unnamed
			buffer.push("function");
			genFunctionBase(node, level);
		}
	}

	function genExpression(
		node: AST.Expression | AST.ColonMemberExpression,
		level: number
	): void {
		adjust(start(node), level);
		switch (node.type) {
			case "Identifier":
				return genIdentifier(node, level);
			case "ParenthesisExpression":
				return genParenthesisExpression(node, level);
			case "BinaryExpression":
			case "LogicalExpression":
				return genBinaryExpression(node, level);
			case "UnaryExpression":
				return genUnaryExpression(node, level);
			case "CallExpression":
				return genCallExpression(node, level);
			case "StringCallExpression":
				return genStringCallExpression(node, level);
			case "TableCallExpression":
				return genTableCallExpression(node, level);
			case "TableConstructorExpression":
				return genTableConstructorExpression(node, level);
			case "FunctionDeclaration":
				return genFunctionDeclaration(node, level);
			case "MemberExpression":
				return genMemberExpression(node, level);
			case "IndexExpression":
				return genIndexExpression(node, level);
			case "StringLiteral":
			case "NumericLiteral":
			case "BooleanLiteral":
			case "VarargLiteral":
			case "NilLiteral":
				return genSimpleLiteral(node, level);
			default:
				throw new Error(`Unknown Expression type ${node.type}`);
		}
	}

	function genIdentifier(node: AST.Identifier, level: number): void {
		adjust(start(node), level);
		buffer.push(node.name);
	}

	function genParenthesisExpression(
		node: AST.ParenthesisExpression,
		level: number
	): void {
		buffer.push("(");
		genExpression(node.expression, level);
		adjust(end(node), level);
		buffer.push(")");
	}

	function genBinaryExpression(
		node: AST.BinaryExpression | AST.LogicalExpression,
		level: number
	): void {
		genExpression(node.left, level);
		buffer.push(" " + node.operator);
		genExpression(node.right, level);
	}

	function genSimpleLiteral(
		node:
			| AST.StringLiteral
			| AST.NilLiteral
			| AST.VarargLiteral
			| AST.BooleanLiteral
			| AST.NumericLiteral,
		level: number
	): void {
		buffer.push(node.raw);
	}

	function genUnaryExpression(node: AST.UnaryExpression, level: number): void {
		buffer.push(node.operator);
		genExpression(node.argument, level);
	}

	function genCallExpression(node: AST.CallExpression, level: number): void {
		genExpression(node.base, level);
		buffer.push("(");
		node.args.forEach(arg => {
			genExpression(arg, level);
			buffer.push(", ");
		});
		if (node.args.length > 0) buffer.pop();
		buffer.push(")");
	}

	function genStringCallExpression(
		node: AST.StringCallExpression,
		level: number
	): void {
		genExpression(node.base, level);
		adjust(start(node.args[0]), level);
		genSimpleLiteral(node.args[0], level);
	}

	function genTableCallExpression(
		node: AST.TableCallExpression,
		level: number
	): void {
		genExpression(node.base, level);
		adjust(start(node.args[0]), level);
		genTableConstructorExpression(node.args[0], level);
	}

	function genMemberExpression(
		node: AST.MemberExpression,
		level: number
	): void {
		genExpression(node.base, level);
		buffer.push(node.indexer);
		genIdentifier(node.identifier, level);
	}

	function genIndexExpression(node: AST.IndexExpression, level: number): void {
		genExpression(node.base, level);
		buffer.push("[");
		genExpression(node.index, level);
		adjust(end(node), level);
		buffer.push("]");
	}

	function genTableField(
		field: AST.TableValue | AST.TableKey | AST.TableKeyString,
		level: number
	): void {
		adjust(start(field), level);
		switch (field.type) {
			case "TableValue":
				return genExpression(field.value, level);
			case "TableKey":
				buffer.push("[");
				genExpression(field.key, level);
				buffer.push("] =");
				genExpression(field.value, level);
				return;
			case "TableKeyString":
				genIdentifier(field.key, level);
				buffer.push(" =");
				genExpression(field.value, level);
				return;
			default:
				throw new Error(`Unknown Table Field type "${field.type}"`);
		}
	}

	function genTableConstructorExpression(
		node: AST.TableConstructorExpression,
		level: number
	): void {
		buffer.push("{");
		node.fields.forEach(f => {
			genTableField(f, level);
			buffer.push(", ");
		});
		if (node.fields.length > 0) buffer.pop();
		adjust(end(node), level);
		buffer.push("}");
	}

	genChunk(ast);
	return buffer.join("");
}
