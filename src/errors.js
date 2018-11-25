// @flow

const StringLiteral = 2;
const Keyword = 4;
const Identifier = 8;
const NumericLiteral = 16;
const Punctuator = 32;
const BooleanLiteral = 64;
const NilLiteral = 128;

// As this parser is a bit different from luas own, the error messages
// will be different in some situations.

export const errors = {
	unexpected: "unexpected %1 '%2' near '%3'",
	expected: "'%1' expected near '%2'",
	expectedToken: "%1 expected near '%2'",
	unfinishedString: "unfinished string near '%1'",
	malformedNumber: "malformed number near '%1'",
	invalidVar: "invalid left-hand side of assignment near '%1'",
	decimalEscapeTooLarge: "decimal escape too large near '%1'",
	invalidEscape: "invalid escape sequence near '%1'",
	hexadecimalDigitExpected: "hexadecimal digit expected near '%1'",
	braceExpected: "missing '%1' near '%2'",
	tooLargeCodepoint: "UTF-8 value too large near '%1'",
	unfinishedLongString:
		"unfinished long string (starting at line %1) near '%2'",
	unfinishedLongComment:
		"unfinished long comment (starting at line %1) near '%2'",
	ambiguousSyntax: "ambiguous syntax (function call x new statement) near '%1'",
};

// ### Error functions

// XXX: Eliminate this function and change the error type to be different from SyntaxError.
// This will unfortunately be a breaking change, because some downstream users depend
// on the error thrown being an instance of SyntaxError. For example, the Ace editor:
// <https://github.com/ajaxorg/ace/blob/4c7e5eb3f5d5ca9434847be51834a4e41661b852/lib/ace/mode/lua_worker.js#L55>

function fixupError(e: any) {
	if (!Object.create) return e;
	return Object.create(e, {
		line: { writable: true, value: e.line },
		index: { writable: true, value: e.index },
		column: { writable: true, value: e.column },
	});
}

// #### Raise an exception.
//
// Raise an exception by passing a token, a string format and its paramters.
//
// The passed tokens location will automatically be added to the error
// message if it exists, if not it will default to the lexers current
// position.
//
// Example:
//
//     // [1:0] expected [ near (
//     raise(token, "expected %1 near %2", '[', token.value);

export function raise(token: any, ...args: Array<any>) {
	const message = sprintf.apply(null, args);
	let error;
	let col;

	if ("undefined" !== typeof token.line) {
		col = token.range[0] - token.lineStart;
		error = fixupError(
			new SyntaxError(sprintf("[%1:%2] %3", token.line, col, message))
		);
		error.line = token.line;
		error.index = token.range[0];
		error.column = col;
	} else {
		error = fixupError(new SyntaxError(message));
	}
	throw error;
}

// #### Raise a general unexpected error
//
// Usage should pass either a token object or a symbol string which was
// expected. We can also specify a nearby token such as <eof>, this will
// default to the currently active token.
//
// Example:
//
//     // Unexpected symbol 'end' near '<eof>'
//     unexpected(token);
//
// If there's no token in the buffer it means we have reached <eof>.

export function unexpected(found: any, near: any) {
	if ("undefined" !== typeof found.type) {
		let type;
		switch (found.type) {
			case StringLiteral:
				type = "string";
				break;
			case Keyword:
				type = "keyword";
				break;
			case Identifier:
				type = "identifier";
				break;
			case NumericLiteral:
				type = "number";
				break;
			case Punctuator:
				type = "symbol";
				break;
			case BooleanLiteral:
				type = "boolean";
				break;
			case NilLiteral:
				return raise(found, errors.unexpected, "symbol", "nil", near);
		}
		return raise(found, errors.unexpected, type, found.value, near);
	}
	return raise(found, errors.unexpected, "symbol", found, near);
}

// A sprintf implementation using %index (beginning at 1) to input
// arguments in the format string.
//
// Example:
//
//     // Unexpected function in token
//     sprintf('Unexpected %2 in %1.', 'token', 'function');

function sprintf(format: any, ...args: Array<any>) {
	const format_ = format.replace(/%(\d)/g, function(match, index) {
		return "" + args[index - 1] || "";
	});
	return format_;
}
