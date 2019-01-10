// @flow

import type { LocationInfo as ASTLoc, Position } from "./ast-types";
import nullthrows from "nullthrows";
import type { LocationInfo as TokenLoc } from "./token-types";

export type MetaInfo = {|
	code: string,
	filename?: string,
|};

function kth(str: string, sub: string, k: number, from?: number = 0): number {
	if (k === 1) return str.indexOf(sub, from);
	else return kth(str, sub, k - 1, str.indexOf(sub, from) + 1);
}

export class CodeError extends Error {
	meta: MetaInfo;
	loc: { start: Position, end?: Position };
	constructor(
		msg: string,
		mi: MetaInfo,
		loc: { start: Position, end?: Position }
	) {
		super();
		this.meta = mi;
		this.loc = loc;
		this.message = msg;
	}

	toString(): string {
		// A bit inefficient but shouldn't be used much
		const fn = this.meta.filename != null ? this.meta.filename : "unknown file";
		const code = this.meta.code;
		const ln = this.loc.start.line;
		const cl = this.loc.start.column;
		const fst_char = ln === 1 ? 0 : kth(code, "\n", ln - 1) + 1;
		let lst_char = kth(code, "\n", ln);
		if (lst_char === -1) lst_char = code.length;

		return `[${fn}:${ln}:${cl}] ${this.message}\n${code.substring(
			fst_char,
			lst_char
		)}`;
	}
}

export function astError(
	msg: string,
	mi: MetaInfo,
	node: { ...ASTLoc }
): CodeError {
	return new CodeError(msg, mi, { ...nullthrows(node.loc) });
}

export function tokenError(
	msg: string,
	mi: MetaInfo,
	node: { ...TokenLoc }
): CodeError {
	const loc: { start: Position, end?: Position } = {
		start: { line: node.line, column: node.range[0] - node.lineStart + 1 },
	};
	if ((node: Object).lastLine != null)
		loc.end = {
			line: (node: Object).lastLine,
			column: (node: Object).lastLineStart,
		};
	return new CodeError(msg, mi, loc);
}

export function unexpectedChar(
	meta: MetaInfo,
	index: number,
	line: number,
	lineStart: number
): CodeError {
	return new CodeError(`unexpected symbol «${meta.code.charAt(index)}»`, meta, {
		start: { line, column: index - lineStart + 1 },
	});
}
