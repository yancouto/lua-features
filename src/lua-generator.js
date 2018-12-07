// @flow strict
/* eslint-disable no-unused-vars */
// Many vars are unused in this file

import * as AST from "./ast-types";

export type GenerateOptions = {|
	// Whether code should be indented. If this is false, the output will be a single line.
	indent: boolean,
|};

const default_options = {
	indent: true,
};

// I'm assuming string concat is efficient and optimized by the compiler
// https://jsperf.com/join-concat/2 && https://www.mail-archive.com/es-discuss@mozilla.org/msg10125.html
export function generate(ast: AST.Chunk, _options?: GenerateOptions): string {
	const options = { ...default_options, ..._options };
	const sep = options.indent ? "\n" : " ";

	function indent(str: string, level: number): string {
		if (options.indent) return "\t".repeat(level) + str;
		else return str;
	}

	function genChunk(node: AST.Chunk): string {
		return genBlock(node.body, 0);
	}

	function genBlock(body: AST.Block, level: number): string {
		return body.map(node => genStatement(node, level)).join(sep);
	}

	function genStatement(node: AST.Statement, level: number): string {
		switch (node.type) {
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

		function genBreakStatement(
			node: AST.BreakStatement,
			level: number
		): string {
			return indent("break", level);
		}

		function genDoStatement(node: AST.DoStatement, level: number): string {
			return `${indent("do", level)}\n${genBlock(
				node.body,
				level + 1
			)}\n${indent("end", level)}`;
		}

		function genLabelStatement(
			node: AST.LabelStatement,
			level: number
		): string {
			return indent(`::${node.label.name}::`, level);
		}

		function genGotoStatement(node: AST.GotoStatement, level: number): string {
			return indent(`goto ${node.label.name}`, level);
		}

		function genCallStatement(node: AST.CallStatement, level: number): string {
			return indent(genExpression(node.expression, level), level);
		}

		function genWhileStatement(
			node: AST.WhileStatement,
			level: number
		): string {
			const id = indent("", level);
			return `${id}while ${genExpression(node.condition, level)} do\n${genBlock(
				node.body,
				level + 1
			)}\n${id}end`;
		}

		function genRepeatStatement(
			node: AST.RepeatStatement,
			level: number
		): string {
			const id = indent("", level);
			return `${id}repeat\n${genBlock(
				node.body,
				level + 1
			)}\n${id}until ${genExpression(node.condition, level)}`;
		}

		function genReturnStatement(
			node: AST.ReturnStatement,
			level: number
		): string {
			return (
				indent("return ", level) +
				node.args.map(exp => genExpression(exp, level)).join(",  ")
			);
		}

		function genIfClause(
			c: AST.IfClause | AST.ElseifClause | AST.ElseClause,
			level: number
		): string {
			const id = indent("", level);
			if (c.type === "ElseClause")
				return indent("else\n", level) + genBlock(c.body, level + 1);
			else
				return (
					indent(c.type === "IfClause" ? "if\n" : "elseif\n", level) +
					genBlock(c.body, level + 1)
				);
		}

		function genIfStatement(node: AST.IfStatement, level: number): string {
			const id = indent("", level);
			return (
				node.clauses.map(c => genIfClause(c, level)).join("\n") + `\n${id}end`
			);
		}

		function genAssignmentStatement(
			node: AST.AssignmentStatement | AST.LocalStatement,
			level: number
		): string {
			let str = indent("", level);
			if (node.type === "LocalStatement")
				str +=
					"local " +
					node.variables.map(v => genExpression(v, level)).join(", ");
			else str += node.variables.map(v => genExpression(v, level)).join(", ");
			if (node.init.length > 0)
				str += ` = ${node.init
					.map(exp => genExpression(exp, level))
					.join(", ")}`;
			return str;
		}

		function genForNumericStatement(
			node: AST.ForNumericStatement,
			level: number
		): string {
			const id = indent("", level);
			let str = `for ${genIdentifier(node.variable, level)} = ${genExpression(
				node.start,
				level
			)}, ${genExpression(node.end, level)}`;
			if (node.step != null) str += `, ${genExpression(node.step, level)}`;
			return `${id}${str} do\n${genBlock(node.body, level + 1)}\n${id}end`;
		}

		function genForGenericStatement(
			node: AST.ForGenericStatement,
			level: number
		): string {
			const id = indent("", level);
			return `${id}for ${node.variables
				.map(v => genIdentifier(v, level))
				.join(", ")} in ${node.iterators
				.map(i => genExpression(i, level))
				.join(", ")} do\n${genBlock(node.body, level + 1)}\n${id}end`;
		}

		function genFunctionBase(
			node: AST.FunctionDeclaration,
			level: number
		): string {
			return `(${node.parameters
				.map(p => genIdentifier(p, level))
				.join(", ")})\n${genBlock(node.body, level + 1)}\n${indent(
				"end",
				level
			)}`;
		}

		function genFunctionName(
			node: AST.NonLocalFunctionName,
			level: number
		): string {
			if (node.type === "Identifier") return genIdentifier(node, level);
			else
				return `${genFunctionName(node.base, level)}${
					node.indexer
				}${genIdentifier(node.identifier, level)}`;
		}

		function genFunctionDeclaration(
			node: AST.FunctionDeclaration,
			level: number
		): string {
			if (node.isLocal) {
				// Local Named
				return indent(
					`local function ${genIdentifier(
						node.identifier,
						level
					)}${genFunctionBase(node, level)}`,
					level
				);
			} else if (node.identifier != null) {
				// NonLocal Named
				return indent(
					`function ${genFunctionName(node.identifier, level)}${genFunctionBase(
						node,
						level
					)}`,
					level
				);
			} else {
				// Unnamed
				return `function${genFunctionBase(node, level)}`;
			}
		}

		function genExpression(
			node: AST.Expression | AST.ColonMemberExpression,
			level: number
		): string {
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

		function genIdentifier(node: AST.Identifier, level: number): string {
			return node.name;
		}

		function genParenthesisExpression(
			node: AST.ParenthesisExpression,
			level: number
		): string {
			return `(${genExpression(node.expression, level)})`;
		}

		function genBinaryExpression(
			node: AST.BinaryExpression | AST.LogicalExpression,
			level: number
		): string {
			return `${genExpression(node.left, level)} ${
				node.operator
			} ${genExpression(node.right, level)}`;
		}

		function genSimpleLiteral(
			node:
				| AST.StringLiteral
				| AST.NilLiteral
				| AST.VarargLiteral
				| AST.BooleanLiteral
				| AST.NumericLiteral,
			level: number
		): string {
			return node.raw;
		}

		function genUnaryExpression(
			node: AST.UnaryExpression,
			level: number
		): string {
			return node.operator + genExpression(node.argument, level);
		}

		function genCallExpression(
			node: AST.CallExpression,
			level: number
		): string {
			return `${genExpression(node.base, level)}(${node.args
				.map(arg => genExpression(arg, level))
				.join(", ")})`;
		}

		function genStringCallExpression(
			node: AST.StringCallExpression,
			level: number
		): string {
			return `${genExpression(node.base, level)} ${genSimpleLiteral(
				node.args[0],
				level
			)}`;
		}

		function genTableCallExpression(
			node: AST.TableCallExpression,
			level: number
		): string {
			return `${genExpression(
				node.base,
				level
			)} ${genTableConstructorExpression(node.args[0], level)}`;
		}

		function genMemberExpression(
			node: AST.MemberExpression,
			level: number
		): string {
			return `${genExpression(node.base, level)}${node.indexer}${genIdentifier(
				node.identifier,
				level
			)}`;
		}

		function genIndexExpression(
			node: AST.IndexExpression,
			level: number
		): string {
			return `${genExpression(node.base, level)}[${genExpression(
				node.index,
				level
			)}]`;
		}

		function genTableField(
			field: AST.TableValue | AST.TableKey | AST.TableKeyString,
			level: number
		): string {
			switch (field.type) {
				case "TableValue":
					return genExpression(field.value, level);
				case "TableKey":
					return `[${genExpression(field.key, level)}] = ${genExpression(
						field.value,
						level
					)}`;
				case "TableKeyString":
					return `${genIdentifier(field.key, level)} = ${genExpression(
						field.value,
						level
					)}`;
				default:
					throw new Error(`Unknown Table Field type "${field.type}"`);
			}
		}

		function genTableConstructorExpression(
			node: AST.TableConstructorExpression,
			level: number
		): string {
			return `{${node.fields
				.map(field => genTableField(field, level))
				.join(", ")}}`;
		}
	}

	return genChunk(ast);
}
