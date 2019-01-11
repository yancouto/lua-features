// @flow

import type { LocationInfo as ASTLoc, MetaInfo, Position } from "./ast-types";
import nullthrows from "nullthrows";
import type { LocationInfo as TokenLoc } from "./token-types";
import util from "util";

export const errors = Object.freeze({
	custom: 0,
	// tokenizer and parser errors
	unexpectedSymbol: 1,
	invalidDelimiter: 2,
	malformedNumber: 3,
	unfinishedString: 4,
	missingInEscape: 5,
	hexDigitExpected: 6,
	tooLargeCodepoint: 7,
	decimalEscapeTooLarge: 8,
	invalidEscape: 9,
	unfinishedLongString: 10,
	unfinishedLongComment: 11,
	expectedType: 12,
	expectedValue: 13,
	invalidVar: 14,
	unexpectedToken: 15,
	ambiguousSyntax: 16,
	invalidVarargs: 17,
	// const error
	reassignConst: 18,
	// type errors
	cannotAssignTypes: 19,
	invalidNumberBinaryOp: 20,
	invalidComparator: 21,
	invalidEqual: 22,
	invalidConcat: 23,
	invalidNumberUnaryOp: 24,
	invalidLen: 25,
	invalidIndex: 26,
});

type ErrorType = $Values<typeof errors>;

const formats = [
	"%s",
	// tokenizer and parser errors
	"unexpected symbol «%s»",
	"invalid long string delimiter",
	"malformed number",
	"unfinished string",
	"missing «%s» in escape sequence",
	"hexadecimal digit expected",
	"UTF-8 value too large",
	"decimal escape too large",
	"invalid escape sequence",
	"unfinished long string (starting at line %s)",
	"unfinished long comment (starting at line %s)",
	"%s expected",
	"«%s» expected",
	"invalid left-hand side of assignment",
	"unexpected",
	"ambiguous syntax (function call x new statement)",
	"cannot use «...» outside a vararg function",
	// const error
	"cannot reassign a constant",
	// type errors
	"cannot assign value of type «%s» to variable of type «%s»",
	"cannot use «%s» with non-number",
	"cannot use «%s» with non-number or string",
	"cannot compare values of different types",
	"cannot use «..» with non-string",
	"cannot use «%s» with non-number",
	"cannot use «#» with non-table",
	"cannot index non-table",
];

function kth(str: string, sub: string, k: number, from?: number = 0): number {
	if (k === 1) return str.indexOf(sub, from);
	else return kth(str, sub, k - 1, str.indexOf(sub, from) + 1);
}

export class CodeError extends Error {
	meta: MetaInfo;
	loc: { start: Position, end?: Position };
	type: ErrorType;
	extra_info: ?Array<string>;
	constructor(
		mi: MetaInfo,
		loc: { start: Position, end?: Position },
		type: ErrorType,
		...extra_info: Array<string>
	) {
		super();
		this.meta = mi;
		this.loc = loc;
		this.type = type;
		this.extra_info = extra_info;
		if (
			this.type === errors.unfinishedLongString ||
			this.type === errors.unfinishedLongComment
		)
			this.extra_info = [`${this.loc.start.line}`];
	}

	toString(): string {
		// A bit inefficient but shouldn't be used much
		const fn = this.meta.filename != null ? this.meta.filename : "unknown file";
		const code = this.meta.code;
		const ln = this.loc.start.line;
		// cl is 1-based
		const cl = this.loc.start.column + 1;
		const fst_char = ln === 1 ? 0 : kth(code, "\n", ln - 1) + 1;
		let lst_char = kth(code, "\n", ln);
		if (lst_char === -1) lst_char = code.length;
		const line = code.slice(fst_char, lst_char);
		const firstNonSpace = line.search(/[^\s]/);
		const indent = 4;

		let highlight = "^".padStart(cl - firstNonSpace + indent);
		let cols = cl;
		if (
			this.loc.end &&
			this.loc.end.line === ln &&
			this.loc.end.column + 1 > cl
		) {
			cols = `${cl}-${this.loc.end.column + 1}`;
			highlight = highlight.padEnd(
				this.loc.end.column - firstNonSpace + indent,
				"^"
			);
		}

		const fmt = formats[this.type];
		const msg =
			this.extra_info != null ? util.format(fmt, ...this.extra_info) : fmt;

		return `[${fn}:${ln}:${cols}] ${msg}\n${" ".repeat(indent)}${line.slice(
			firstNonSpace
		)}\n${highlight}`;
	}
}

export function astError(
	type: ErrorType,
	mi: MetaInfo,
	node: { ...ASTLoc },
	...extra_info: Array<string>
): CodeError {
	return new CodeError(mi, { ...nullthrows(node.loc) }, type, ...extra_info);
}

export function tokenError(
	type: ErrorType,
	mi: MetaInfo,
	node: { ...TokenLoc },
	...extra_info: Array<string>
): CodeError {
	const err = mi.code.slice(node.range[0], node.range[1]);
	const loc: { start: Position, end?: Position } = {
		start: { line: node.line, column: node.range[0] - node.lineStart },
		end: {
			line: node.line + (err.match(/\n/g) || []).length,
			column: node.range[1] - mi.code.lastIndexOf("\n", node.range[1] - 1) - 1,
		},
	};
	return new CodeError(mi, loc, type, ...extra_info);
}
