// @flow

import * as AST from "./ast-types";

export type LuaParseOptions = {|
	+wait?: boolean,
	+comments?: boolean,
	+locations?: boolean,
	+ranges?: boolean,
	+extendedIdentifiers?: boolean,
	+onCreateNode?: any => any,
	+luaVersion?: "5.1" | "5.2" | "5.3" | "LuaJIT",
|};

let input, options, length, features;
// Options can be set either globally on the parser object through
// defaultOptions, or during the parse call.
const defaultOptions = {
	// Explicitly tell the parser when the input ends.
	wait: false,
	// Store comments as an array in the chunk object.
	comments: true,
	// Store location information on each syntax node as
	// `loc: { start: { line, column }, end: { line, column } }`.
	locations: false,
	// Store the start and end character locations on each syntax node as
	// `range: [start, end]`.
	ranges: false,
	// A callback which will be invoked when a syntax node has been completed.
	// The node which has been created will be passed as the only parameter.
	onCreateNode: null,
	// The version of Lua targeted by the parser (string; allowed values are
	// '5.1', '5.2', '5.3').
	luaVersion: "5.1",
	// Whether to allow code points outside the Basic Latin block in identifiers
	extendedIdentifiers: false,
};

// The available tokens expressed as enum flags so they can be checked with
// bitwise operations.

const EOF = 1,
	StringLiteral = 2,
	Keyword = 4,
	Identifier = 8,
	NumericLiteral = 16,
	Punctuator = 32,
	BooleanLiteral = 64,
	NilLiteral = 128,
	VarargLiteral = 256;

// As this parser is a bit different from luas own, the error messages
// will be different in some situations.

const errors = {
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

// ### Abstract Syntax Tree
//
// The default AST structure is inspired by the Mozilla Parser API but can
// easily be customized by overriding these functions.

const ast = {
	labelStatement: function(label: any) {
		return {
			type: "LabelStatement",
			label: label,
		};
	},

	breakStatement: function() {
		return {
			type: "BreakStatement",
		};
	},

	gotoStatement: function(label: any) {
		return {
			type: "GotoStatement",
			label: label,
		};
	},

	returnStatement: function(args: any) {
		return {
			type: "ReturnStatement",
			args: args,
		};
	},

	ifStatement: function(clauses: any) {
		return {
			type: "IfStatement",
			clauses: clauses,
		};
	},
	ifClause: function(condition: any, body: any) {
		return {
			type: "IfClause",
			condition: condition,
			body: body,
		};
	},
	elseifClause: function(condition: any, body: any) {
		return {
			type: "ElseifClause",
			condition: condition,
			body: body,
		};
	},
	elseClause: function(body: any) {
		return {
			type: "ElseClause",
			body: body,
		};
	},

	whileStatement: function(condition: any, body: any) {
		return {
			type: "WhileStatement",
			condition: condition,
			body: body,
		};
	},

	doStatement: function(body: any) {
		return {
			type: "DoStatement",
			body: body,
		};
	},

	repeatStatement: function(condition: any, body: any) {
		return {
			type: "RepeatStatement",
			condition: condition,
			body: body,
		};
	},

	localStatement: function(variables: any, typeList: any, init: any) {
		return {
			type: "LocalStatement",
			variables: variables,
			typeList: typeList,
			init: init,
		};
	},

	assignmentStatement: function(variables: any, init: any) {
		return {
			type: "AssignmentStatement",
			variables: variables,
			init: init,
		};
	},

	callStatement: function(expression: any) {
		return {
			type: "CallStatement",
			expression: expression,
		};
	},

	functionStatement: function(
		identifier: any,
		parameters: any,
		parameter_types: any,
		return_types: any,
		hasVarargs: any,
		isLocal: any,
		body: any
	) {
		return {
			type: "FunctionDeclaration",
			identifier: identifier,
			isLocal: isLocal,
			parameters: parameters,
			parameter_types: parameter_types,
			return_types: return_types,
			hasVarargs: hasVarargs,
			body: body,
		};
	},

	forNumericStatement: function(
		variable: any,
		start: any,
		end: any,
		step: any,
		body: any
	) {
		return {
			type: "ForNumericStatement",
			variable: variable,
			start: start,
			end: end,
			step: step,
			body: body,
		};
	},

	forGenericStatement: function(variables: any, iterators: any, body: any) {
		return {
			type: "ForGenericStatement",
			variables: variables,
			iterators: iterators,
			body: body,
		};
	},

	chunk: function(body: any) {
		return {
			type: "Chunk",
			body: body,
		};
	},

	identifier: function(name: any) {
		return {
			type: "Identifier",
			name: name,
		};
	},

	functionType: function(parameters: any, returns: any) {
		return {
			type: "FunctionType",
			parameter_types: parameters,
			return_types: returns,
		};
	},

	tableType: function(typeMap: any) {
		return {
			type: "TableType",
			typeMap: typeMap,
		};
	},

	simpleType: function(value: any) {
		return {
			type: "SimpleType",
			value: value,
		};
	},

	typeInfo: function(possibleTypes: any) {
		return {
			type: "TypeInfo",
			possibleTypes,
		};
	},

	typeList: function(list: any, rest: any) {
		return {
			type: "TypeList",
			list: list,
			rest: rest,
		};
	},

	literal: function(type: any, value: any, raw: any) {
		const type_str =
			type === StringLiteral
				? "StringLiteral"
				: type === NumericLiteral
				? "NumericLiteral"
				: type === BooleanLiteral
				? "BooleanLiteral"
				: type === NilLiteral
				? "NilLiteral"
				: "VarargLiteral";

		return {
			type: type_str,
			value: value,
			raw: raw,
		};
	},

	parenthesisExpression: function(expression: any) {
		return {
			type: "ParenthesisExpression",
			expression: expression,
		};
	},

	tableKey: function(key: any, value: any) {
		return {
			type: "TableKey",
			key: key,
			value: value,
		};
	},
	tableKeyString: function(key: any, value: any) {
		return {
			type: "TableKeyString",
			key: key,
			value: value,
		};
	},
	tableValue: function(value: any) {
		return {
			type: "TableValue",
			value: value,
		};
	},

	tableConstructorExpression: function(fields: any) {
		return {
			type: "TableConstructorExpression",
			fields: fields,
		};
	},
	binaryExpression: function(operator: any, left: any, right: any) {
		const type =
			"and" === operator || "or" === operator
				? "LogicalExpression"
				: "BinaryExpression";

		return {
			type: type,
			operator: operator,
			left: left,
			right: right,
		};
	},
	unaryExpression: function(operator: any, argument: any) {
		return {
			type: "UnaryExpression",
			operator: operator,
			argument: argument,
		};
	},
	memberExpression: function(base: any, indexer: any, identifier: any) {
		return {
			type: "MemberExpression",
			indexer: indexer,
			identifier: identifier,
			base: base,
		};
	},

	indexExpression: function(base: any, index: any) {
		return {
			type: "IndexExpression",
			base: base,
			index: index,
		};
	},

	callExpression: function(base: any, args: any) {
		return {
			type: "CallExpression",
			base: base,
			args: args,
		};
	},

	tableCallExpression: function(base: any, args: any) {
		return {
			type: "TableCallExpression",
			base: base,
			args: [args],
		};
	},

	stringCallExpression: function(base: any, argument: any) {
		return {
			type: "StringCallExpression",
			base: base,
			args: [argument],
		};
	},

	comment: function(value: any, raw: any) {
		return {
			type: "Comment",
			value: value,
			raw: raw,
		};
	},
};

const nil_type = ast.typeInfo(new Set([ast.simpleType("nil")]));
const any_type = ast.typeInfo(new Set([ast.simpleType("any")]));

// Wrap up the node object.

function finishNode(node: any) {
	// Pop a `Marker` off the location-array and attach its location data.
	if (trackLocations) {
		const location = locations.pop();
		location.complete();
		location.bless(node);
	}
	if (options.onCreateNode) options.onCreateNode(node);
	return node;
}

// Helpers
// -------

const indexOf = function indexOf(array, element) {
	for (let i = 0, length = array.length; i < length; ++i) {
		if (array[i] === element) return i;
	}
	return -1;
};

// Iterate through an array of objects and return the index of an object
// with a matching property.

function indexOfObject(array, property, element) {
	for (let i = 0, length = array.length; i < length; ++i) {
		if (array[i][property] === element) return i;
	}
	return -1;
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

// Returns a new object with the properties from all objectes passed as
// arguments. Last argument takes precedence.
//
// Example:
//
//     this.options = extend(options, { output: false });

function extend(...args: Array<any>) {
	const dest = {};
	let src;
	let prop;

	for (let i = 0, length = args.length; i < length; ++i) {
		src = args[i];
		for (prop in src)
			if (src.hasOwnProperty(prop)) {
				dest[prop] = src[prop];
			}
	}
	return dest;
}

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

function raise(token: any, ...args: Array<any>) {
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
		col = index - lineStart + 1;
		error = fixupError(
			new SyntaxError(sprintf("[%1:%2] %3", line, col, message))
		);
		error.index = index;
		error.line = line;
		error.column = col;
	}
	throw error;
}

// #### Raise an unexpected token error.
//
// Example:
//
//     // expected <name> near '0'
//     raiseUnexpectedToken('<name>', token);

function raiseUnexpectedToken(type: any, token: any) {
	raise(token, errors.expectedToken, type, token.value);
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

function unexpected(found: any) {
	const near = lookahead.value;
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

// Lexer
// -----
//
// The lexer, or the tokenizer reads the input string character by character
// and derives a token left-right. To be as efficient as possible the lexer
// prioritizes the common cases such as identifiers. It also works with
// character codes instead of characters as string comparisons was the
// biggest bottleneck of the parser.
//
// If `options.comments` is enabled, all comments encountered will be stored
// in an array which later will be appended to the chunk object. If disabled,
// they will simply be disregarded.
//
// When the lexer has derived a valid token, it will be returned as an object
// containing its value and as well as its position in the input string (this
// is always enabled to provide proper debug messages).
//
// `lex()` starts lexing and returns the following token in the stream.

let index,
	token,
	previousToken: any,
	lookahead,
	comments,
	tokenStart,
	line,
	lineStart;

function lex() {
	skipWhiteSpace();

	// Skip comments beginning with --
	while (45 === input.charCodeAt(index) && 45 === input.charCodeAt(index + 1)) {
		scanComment();
		skipWhiteSpace();
	}
	if (index >= length)
		return {
			type: EOF,
			value: "<eof>",
			line: line,
			lineStart: lineStart,
			range: [index, index],
		};

	const charCode = input.charCodeAt(index),
		next = input.charCodeAt(index + 1);

	// Memorize the range index where the token begins.
	tokenStart = index;
	if (isIdentifierStart(charCode)) return scanIdentifierOrKeyword();

	switch (charCode) {
		case 39:
		case 34: // '"
			return scanStringLiteral();

		case 48:
		case 49:
		case 50:
		case 51:
		case 52:
		case 53:
		case 54:
		case 55:
		case 56:
		case 57: // 0-9
			return scanNumericLiteral();

		case 46: // .
			// If the dot is followed by a digit it's a float.
			if (isDecDigit(next)) return scanNumericLiteral();
			if (46 === next) {
				if (46 === input.charCodeAt(index + 2)) return scanVarargLiteral();
				return scanPunctuator("..");
			}
			return scanPunctuator(".");

		case 61: // =
			if (61 === next) return scanPunctuator("==");
			if (62 === next) return scanPunctuator("=>");
			return scanPunctuator("=");

		case 62: // >
			if (features.bitwiseOperators)
				if (62 === next) return scanPunctuator(">>");
			if (61 === next) return scanPunctuator(">=");
			return scanPunctuator(">");

		case 60: // <
			if (features.bitwiseOperators)
				if (60 === next) return scanPunctuator("<<");
			if (61 === next) return scanPunctuator("<=");
			return scanPunctuator("<");

		case 126: // ~
			if (61 === next) return scanPunctuator("~=");
			if (!features.bitwiseOperators) break;
			return scanPunctuator("~");

		case 58: // :
			if (features.labels) if (58 === next) return scanPunctuator("::");
			return scanPunctuator(":");

		case 91: // [
			// Check for a multiline string, they begin with [= or [[
			if (91 === next || 61 === next) return scanLongStringLiteral();
			return scanPunctuator("[");

		case 47: // /
			// Check for integer division op (//)
			if (features.integerDivision)
				if (47 === next) return scanPunctuator("//");
			return scanPunctuator("/");

		case 38:
		case 124: // & |
			if (!features.bitwiseOperators) break;

		/* fall through */
		case 42:
		case 94:
		case 37:
		case 44:
		case 123:
		case 125:
		case 93:
		case 40:
		case 41:
		case 59:
		case 35:
		case 45:
		case 43: // * ^ % , { } ] ( ) ; # - +
			return scanPunctuator(input.charAt(index));
	}

	return unexpected(input.charAt(index));
}

// Whitespace has no semantic meaning in lua so simply skip ahead while
// tracking the encounted newlines. Any kind of eol sequence is counted as a
// single line.

function consumeEOL() {
	const charCode = input.charCodeAt(index),
		peekCharCode = input.charCodeAt(index + 1);

	if (isLineTerminator(charCode)) {
		// Count \n\r and \r\n as one newline.
		if (10 === charCode && 13 === peekCharCode) ++index;
		if (13 === charCode && 10 === peekCharCode) ++index;
		++line;
		lineStart = ++index;

		return true;
	}
	return false;
}

function skipWhiteSpace() {
	while (index < length) {
		const charCode = input.charCodeAt(index);
		if (isWhiteSpace(charCode)) {
			++index;
		} else if (!consumeEOL()) {
			break;
		}
	}
}

function encodeUTF8(codepoint) {
	if (codepoint < 0x80) {
		return String.fromCharCode(codepoint);
	} else if (codepoint < 0x800) {
		return String.fromCharCode(
			0xc0 | (codepoint >> 6),
			0x80 | (codepoint & 0x3f)
		);
	} else if (codepoint < 0x10000) {
		return String.fromCharCode(
			0xe0 | (codepoint >> 12),
			0x80 | ((codepoint >> 6) & 0x3f),
			0x80 | (codepoint & 0x3f)
		);
	} else if (codepoint < 0x110000) {
		return String.fromCharCode(
			0xf0 | (codepoint >> 18),
			0x80 | ((codepoint >> 12) & 0x3f),
			0x80 | ((codepoint >> 6) & 0x3f),
			0x80 | (codepoint & 0x3f)
		);
	} else {
		return null;
	}
}

// This function takes a JavaScript string, encodes it in WTF-8 and
// reinterprets the resulting code units as code points; i.e. it encodes
// the string in what was the original meaning of WTF-8.
//
// For a detailed rationale, see the README.md file, section
// "Note on character encodings".

function fixupHighCharacters(s: any) {
	// eslint-disable-next-line no-control-regex
	return s.replace(/[\ud800-\udbff][\udc00-\udfff]|[^\x00-\x7f]/g, function(m) {
		if (m.length === 1) return encodeUTF8(m.charCodeAt(0));
		return encodeUTF8(
			0x10000 + (((m.charCodeAt(0) & 0x3ff) << 10) | (m.charCodeAt(1) & 0x3ff))
		);
	});
}

// Identifiers, keywords, booleans and nil all look the same syntax wise. We
// simply go through them one by one and defaulting to an identifier if no
// previous case matched.

function scanIdentifierOrKeyword() {
	let value, type;

	// Slicing the input string is prefered before string concatenation in a
	// loop for performance reasons.
	while (isIdentifierPart(input.charCodeAt(++index)));
	value = fixupHighCharacters(input.slice(tokenStart, index));

	// Decide on the token type and possibly cast the value.
	if (isKeyword(value)) {
		type = Keyword;
	} else if ("true" === value || "false" === value) {
		type = BooleanLiteral;
		value = "true" === value;
	} else if ("nil" === value) {
		type = NilLiteral;
		value = null;
	} else {
		type = Identifier;
	}

	return {
		type: type,
		value: value,
		line: line,
		lineStart: lineStart,
		range: [tokenStart, index],
	};
}

// Once a punctuator reaches this function it should already have been
// validated so we simply return it as a token.

function scanPunctuator(value) {
	index += value.length;
	return {
		type: Punctuator,
		value: value,
		line: line,
		lineStart: lineStart,
		range: [tokenStart, index],
	};
}

// A vararg literal consists of three dots.

function scanVarargLiteral() {
	index += 3;
	return {
		type: VarargLiteral,
		value: "...",
		line: line,
		lineStart: lineStart,
		range: [tokenStart, index],
	};
}

// Find the string literal by matching the delimiter marks used.

function scanStringLiteral() {
	const delimiter = input.charCodeAt(index++);
	const beginLine = line;
	const beginLineStart = lineStart;
	let stringStart = index;
	let string = "";
	let charCode;

	while (index < length) {
		charCode = input.charCodeAt(index++);
		if (delimiter === charCode) break;
		if (92 === charCode) {
			// backslash
			string +=
				fixupHighCharacters(input.slice(stringStart, index - 1)) +
				readEscapeSequence();
			stringStart = index;
		}
		// EOF or `\n` terminates a string literal. If we haven't found the
		// ending delimiter by now, raise an exception.
		if (index >= length || isLineTerminator(charCode)) {
			string += input.slice(stringStart, index - 1);
			raise(
				{},
				errors.unfinishedString,
				string + String.fromCharCode(charCode)
			);
		}
	}
	string += fixupHighCharacters(input.slice(stringStart, index - 1));

	return {
		type: StringLiteral,
		value: string,
		line: beginLine,
		lineStart: beginLineStart,
		lastLine: line,
		lastLineStart: lineStart,
		range: [tokenStart, index],
	};
}

// Expect a multiline string literal and return it as a regular string
// literal, if it doesn't validate into a valid multiline string, throw an
// exception.

function scanLongStringLiteral() {
	const beginLine = line,
		beginLineStart = lineStart,
		string = readLongString(false);
	// Fail if it's not a multiline literal.
	if (false === string) raise(token, errors.expected, "[", token.value);

	return {
		type: StringLiteral,
		value: fixupHighCharacters(string),
		line: beginLine,
		lineStart: beginLineStart,
		lastLine: line,
		lastLineStart: lineStart,
		range: [tokenStart, index],
	};
}

// Numeric literals will be returned as floating-point numbers instead of
// strings. The raw value should be retrieved from slicing the input string
// later on in the process.
//
// If a hexadecimal number is encountered, it will be converted.

function scanNumericLiteral() {
	const character = input.charAt(index),
		next = input.charAt(index + 1);

	const value =
		"0" === character && ("xX": any).indexOf(next || null) >= 0
			? readHexLiteral()
			: readDecLiteral();

	return {
		type: NumericLiteral,
		value: value,
		line: line,
		lineStart: lineStart,
		range: [tokenStart, index],
	};
}

// Lua hexadecimals have an optional fraction part and an optional binary
// exoponent part. These are not included in JavaScript so we will compute
// all three parts separately and then sum them up at the end of the function
// with the following algorithm.
//
//     Digit := toDec(digit)
//     Fraction := toDec(fraction) / 16 ^ fractionCount
//     BinaryExp := 2 ^ binaryExp
//     Number := ( Digit + Fraction ) * BinaryExp

function readHexLiteral() {
	let fraction = 0; // defaults to 0 as it gets summed
	let binaryExponent = 1; // defaults to 1 as it gets multiplied
	let binarySign = 1; // positive
	let fractionStart;
	let exponentStart;

	const digitStart = (index += 2); // Skip 0x part

	// A minimum of one hex digit is required.
	if (!isHexDigit(input.charCodeAt(index)))
		raise({}, errors.malformedNumber, input.slice(tokenStart, index));

	while (isHexDigit(input.charCodeAt(index))) ++index;
	// Convert the hexadecimal digit to base 10.
	const digit = parseInt(input.slice(digitStart, index), 16);

	// Fraction part i optional.
	if ("." === input.charAt(index)) {
		fractionStart = ++index;

		while (isHexDigit(input.charCodeAt(index))) ++index;
		fraction = input.slice(fractionStart, index);

		// Empty fraction parts should default to 0, others should be converted
		// 0.x form so we can use summation at the end.
		fraction =
			fractionStart === index
				? 0
				: parseInt(fraction, 16) / Math.pow(16, index - fractionStart);
	}

	// Binary exponents are optional
	if (("pP": any).indexOf(input.charAt(index) || null) >= 0) {
		++index;

		// Sign part is optional and defaults to 1 (positive).
		if (("+-": any).indexOf(input.charAt(index) || null) >= 0)
			binarySign = "+" === input.charAt(index++) ? 1 : -1;

		exponentStart = index;

		// The binary exponent sign requires a decimal digit.
		if (!isDecDigit(input.charCodeAt(index)))
			raise({}, errors.malformedNumber, input.slice(tokenStart, index));

		while (isDecDigit(input.charCodeAt(index))) ++index;
		binaryExponent = input.slice(exponentStart, index);

		// Calculate the binary exponent of the number.
		binaryExponent = Math.pow(2, (binaryExponent: any) * binarySign);
	}

	return (digit + fraction) * binaryExponent;
}

// Decimal numbers are exactly the same in Lua and in JavaScript, because of
// this we check where the token ends and then parse it with native
// functions.

function readDecLiteral() {
	while (isDecDigit(input.charCodeAt(index))) ++index;
	// Fraction part is optional
	if ("." === input.charAt(index)) {
		++index;
		// Fraction part defaults to 0
		while (isDecDigit(input.charCodeAt(index))) ++index;
	}
	// Exponent part is optional.
	if (("eE": any).indexOf(input.charAt(index) || null) >= 0) {
		++index;
		// Sign part is optional.
		if (("+-": any).indexOf(input.charAt(index) || null) >= 0) ++index;
		// An exponent is required to contain at least one decimal digit.
		if (!isDecDigit(input.charCodeAt(index)))
			raise({}, errors.malformedNumber, input.slice(tokenStart, index));

		while (isDecDigit(input.charCodeAt(index))) ++index;
	}

	return parseFloat(input.slice(tokenStart, index));
}

function readUnicodeEscapeSequence() {
	const sequenceStart = index++;

	if (input.charAt(index++) !== "{")
		raise(
			{},
			errors.braceExpected,
			"{",
			"\\" + input.slice(sequenceStart, index)
		);
	if (!isHexDigit(input.charCodeAt(index)))
		raise(
			{},
			errors.hexadecimalDigitExpected,
			"\\" + input.slice(sequenceStart, index)
		);

	while (input.charCodeAt(index) === 0x30) ++index;
	const escStart = index;

	while (isHexDigit(input.charCodeAt(index))) {
		++index;
		if (index - escStart > 6)
			raise(
				{},
				errors.tooLargeCodepoint,
				"\\" + input.slice(sequenceStart, index)
			);
	}

	const b = input.charAt(index++);
	if (b !== "}") {
		if (b === '"' || b === "'")
			raise(
				{},
				errors.braceExpected,
				"}",
				"\\" + input.slice(sequenceStart, index--)
			);
		else
			raise(
				{},
				errors.hexadecimalDigitExpected,
				"\\" + input.slice(sequenceStart, index)
			);
	}

	let codepoint = parseInt(input.slice(escStart, index - 1), 16);

	codepoint = encodeUTF8(codepoint);
	if (codepoint === null) {
		raise(
			{},
			errors.tooLargeCodepoint,
			"\\" + input.slice(sequenceStart, index)
		);
	}
	return codepoint;
}

// Translate escape sequences to the actual characters.
function readEscapeSequence() {
	const sequenceStart = index;
	switch (input.charAt(index)) {
		// Lua allow the following escape sequences.
		case "a":
			++index;
			return "\x07";
		case "n":
			++index;
			return "\n";
		case "r":
			++index;
			return "\r";
		case "t":
			++index;
			return "\t";
		case "v":
			++index;
			return "\x0b";
		case "b":
			++index;
			return "\b";
		case "f":
			++index;
			return "\f";

		// Backslash at the end of the line. We treat all line endings as equivalent,
		// and as representing the [LF] character (code 10). Lua 5.1 through 5.3
		// have been verified to behave the same way.
		case "\r":
		case "\n":
			consumeEOL();
			return "\n";

		case "0":
		case "1":
		case "2":
		case "3":
		case "4":
		case "5":
		case "6":
		case "7":
		case "8":
		case "9": {
			// \ddd, where ddd is a sequence of up to three decimal digits.
			while (isDecDigit(input.charCodeAt(index)) && index - sequenceStart < 3)
				++index;

			const ddd = parseInt(input.slice(sequenceStart, index), 10);
			if (ddd > 255) {
				raise({}, errors.decimalEscapeTooLarge, "\\" + ddd);
			}
			return String.fromCharCode(ddd);
		}

		case "z":
			if (features.skipWhitespaceEscape) {
				++index;
				skipWhiteSpace();
				return "";
			}

		/* fall through */
		case "x":
			if (features.hexEscapes) {
				// \xXX, where XX is a sequence of exactly two hexadecimal digits
				if (
					isHexDigit(input.charCodeAt(index + 1)) &&
					isHexDigit(input.charCodeAt(index + 2))
				) {
					index += 3;
					return String.fromCharCode(
						parseInt(input.slice(sequenceStart + 1, index), 16)
					);
				}
				raise(
					{},
					errors.hexadecimalDigitExpected,
					"\\" + input.slice(sequenceStart, index + 2)
				);
			}

		/* fall through */
		case "u":
			if (features.unicodeEscapes) {
				return readUnicodeEscapeSequence();
			}

		/* fall through */
		default:
			if (features.strictEscapes)
				raise(
					{},
					errors.invalidEscape,
					"\\" + input.slice(sequenceStart, index + 1)
				);

		/* fall through */
		case "\\":
		case '"':
		case "'":
			return input.charAt(index++);
	}
}

// Comments begin with -- after which it will be decided if they are
// multiline comments or not.
//
// The multiline functionality works the exact same way as with string
// literals so we reuse the functionality.

function scanComment() {
	tokenStart = index;
	index += 2; // --

	const character = input.charAt(index);
	let content = "";
	let isLong = false;
	const commentStart = index;
	const lineStartComment = lineStart;
	const lineComment = line;

	if ("[" === character) {
		content = readLongString(true);
		// This wasn't a multiline comment after all.
		if (false === content) content = character;
		else isLong = true;
	}
	// Scan until next line as long as it's not a multiline comment.
	if (!isLong) {
		while (index < length) {
			if (isLineTerminator(input.charCodeAt(index))) break;
			++index;
		}
		if (options.comments) content = input.slice(commentStart, index);
	}

	if (options.comments) {
		const node: any = ast.comment(content, input.slice(tokenStart, index));

		// `Marker`s depend on tokens available in the parser and as comments are
		// intercepted in the lexer all location data is set manually.
		if (options.locations) {
			node.loc = {
				start: { line: lineComment, column: tokenStart - lineStartComment },
				end: { line: line, column: index - lineStart },
			};
		}
		if (options.ranges) {
			node.range = [tokenStart, index];
		}
		if (options.onCreateNode) options.onCreateNode(node);
		comments.push(node);
	}
}

// Read a multiline string by calculating the depth of `=` characters and
// then appending until an equal depth is found.

function readLongString(isComment) {
	let level = 0;
	let content = "";
	let terminator = false;
	let character;
	const firstLine = line;

	++index; // [

	// Calculate the depth of the comment.
	while ("=" === input.charAt(index + level)) ++level;
	// Exit, this is not a long string afterall.
	if ("[" !== input.charAt(index + level)) return false;

	index += level + 1;

	// If the first character is a newline, ignore it and begin on next line.
	if (isLineTerminator(input.charCodeAt(index))) consumeEOL();

	const stringStart = index;
	while (index < length) {
		// To keep track of line numbers run the `consumeEOL()` which increments
		// its counter.
		while (isLineTerminator(input.charCodeAt(index))) consumeEOL();

		character = input.charAt(index++);

		// Once the delimiter is found, iterate through the depth count and see
		// if it matches.
		if ("]" === character) {
			terminator = true;
			for (let i = 0; i < level; ++i) {
				if ("=" !== input.charAt(index + i)) terminator = false;
			}
			if ("]" !== input.charAt(index + level)) terminator = false;
		}

		// We reached the end of the multiline string. Get out now.
		if (terminator) {
			content += input.slice(stringStart, index - 1);
			index += level + 1;
			return content;
		}
	}

	raise(
		{},
		isComment ? errors.unfinishedLongComment : errors.unfinishedLongString,
		firstLine,
		"<eof>"
	);
}

// ## Lex functions and helpers.

// Read the next token.
//
// This is actually done by setting the current token to the lookahead and
// reading in the new lookahead token.

function next() {
	previousToken = token;
	token = lookahead;
	lookahead = lex();
}

// Consume a token if its value matches. Once consumed or not, return the
// success of the operation.

function consume(value) {
	if (value === token.value) {
		next();
		return true;
	}
	return false;
}

// Expect the next token value to match. If not, throw an exception.

function expect(value) {
	if (value === token.value) next();
	else raise(token, errors.expected, value, token.value);
}

// ### Validation functions

function isWhiteSpace(charCode) {
	return (
		9 === charCode || 32 === charCode || 0xb === charCode || 0xc === charCode
	);
}

function isLineTerminator(charCode) {
	return 10 === charCode || 13 === charCode;
}

function isDecDigit(charCode) {
	return charCode >= 48 && charCode <= 57;
}

function isHexDigit(charCode) {
	return (
		(charCode >= 48 && charCode <= 57) ||
		(charCode >= 97 && charCode <= 102) ||
		(charCode >= 65 && charCode <= 70)
	);
}

// From [Lua 5.2](http://www.lua.org/manual/5.2/manual.html#8.1) onwards
// identifiers cannot use 'locale-dependent' letters (i.e. dependent on the C locale).
// On the other hand, LuaJIT allows arbitrary octets ≥ 128 in identifiers.

function isIdentifierStart(charCode) {
	if (
		(charCode >= 65 && charCode <= 90) ||
		(charCode >= 97 && charCode <= 122) ||
		95 === charCode
	)
		return true;
	if (options.extendedIdentifiers && charCode >= 128) return true;
	return false;
}

function isIdentifierPart(charCode) {
	if (
		(charCode >= 65 && charCode <= 90) ||
		(charCode >= 97 && charCode <= 122) ||
		95 === charCode ||
		(charCode >= 48 && charCode <= 57)
	)
		return true;
	if (options.extendedIdentifiers && charCode >= 128) return true;
	return false;
}

// [3.1 Lexical Conventions](http://www.lua.org/manual/5.2/manual.html#3.1)
//
// `true`, `false` and `nil` will not be considered keywords, but literals.

function isKeyword(id) {
	switch (id.length) {
		case 2:
			return "do" === id || "if" === id || "in" === id || "or" === id;
		case 3:
			return "and" === id || "end" === id || "for" === id || "not" === id;
		case 4:
			if ("else" === id || "then" === id) return true;
			if (features.labels && !features.contextualGoto) return "goto" === id;
			return false;
		case 5:
			return (
				"break" === id || "local" === id || "until" === id || "while" === id
			);
		case 6:
			return "elseif" === id || "repeat" === id || "return" === id;
		case 8:
			return "function" === id;
	}
	return false;
}

function isUnary(token) {
	if (Punctuator === token.type) return "#-~".indexOf(token.value) >= 0;
	if (Keyword === token.type) return "not" === token.value;
	return false;
}

// @TODO this needs to be rethought.
function isCallExpression(expression) {
	switch (expression.type) {
		case "CallExpression":
		case "TableCallExpression":
		case "StringCallExpression":
			return true;
	}
	return false;
}

// Check if the token syntactically closes a block.

function isBlockFollow(token) {
	if (EOF === token.type) return true;
	if (Keyword !== token.type) return false;
	switch (token.value) {
		case "else":
		case "elseif":
		case "end":
		case "until":
			return true;
		default:
			return false;
	}
}

// Scope
// -----

// Store each block scope as a an array of identifier names. Each scope is
// stored in an FILO-array.
let scopes,
	function_scope,
	// The current scope index
	scopeDepth,
	// A list of all global identifier nodes.
	globals;

// Create a new scope inheriting all declarations from the previous scope.
function createScope(isFunction) {
	scopeDepth++;
	scopes.push([]);
	function_scope.push(isFunction);
}

// Exit and remove the current scope.
function destroyScope() {
	scopes.pop();
	function_scope.pop();
	scopeDepth--;
}

// Add identifier name to the current scope if it doesnt already exist.
function scopeIdentifierName(name) {
	if (-1 !== indexOf(scopes[scopeDepth], name)) return;
	scopes[scopeDepth].push(name);
}

// Add identifier to the current scope
function scopeIdentifier(node) {
	scopeIdentifierName(node.name);
	attachScope(node, true);
}

// Attach scope information to node. If the node is global, store it in the
// globals array so we can return the information to the user.
function attachScope(node, isLocal) {
	if (!isLocal && -1 === indexOfObject(globals, "name", node.name))
		globals.push(node);

	node.isLocal = isLocal;
}

// Is the identifier name available in this scope.
function scopeHasName(name) {
	// TODO: simplify this, since it always looks for ...
	for (let i = scopeDepth; i >= 0; i--) {
		if (-1 !== indexOf(scopes[i], name)) return true;
		if (function_scope[i]) break;
	}
	return false;
}

// Location tracking
// -----------------
//
// Locations are stored in FILO-array as a `Marker` object consisting of both
// `loc` and `range` data. Once a `Marker` is popped off the list an end
// location is added and the data is attached to a syntax node.

let locations: Array<any> = [],
	trackLocations: any;

function createLocationMarker() {
	return new Marker(token);
}

function Marker(token) {
	if (options.locations) {
		this.loc = {
			start: {
				line: token.line,
				column: token.range[0] - token.lineStart,
			},
			end: {
				line: 0,
				column: 0,
			},
		};
	}
	if (options.ranges) this.range = [token.range[0], 0];
}

// Complete the location data stored in the `Marker` by adding the location
// of the *previous token* as an end location.
Marker.prototype.complete = function() {
	if (options.locations) {
		this.loc.end.line = previousToken.lastLine || previousToken.line;
		this.loc.end.column =
			previousToken.range[1] -
			(previousToken.lastLineStart || previousToken.lineStart);
	}
	if (options.ranges) {
		this.range[1] = previousToken.range[1];
	}
};

Marker.prototype.bless = function(node) {
	if (this.loc) {
		const loc = this.loc;
		node.loc = {
			start: {
				line: loc.start.line,
				column: loc.start.column,
			},
			end: {
				line: loc.end.line,
				column: loc.end.column,
			},
		};
	}
	if (this.range) {
		node.range = [this.range[0], this.range[1]];
	}
};

// Create a new `Marker` and add it to the FILO-array.
function markLocation() {
	if (trackLocations) locations.push(createLocationMarker());
}

// Push an arbitrary `Marker` object onto the FILO-array.
function pushLocation(marker) {
	if (trackLocations) locations.push(marker);
}

// Parse functions
// ---------------

// Chunk is the main program object. Syntactically it's the same as a block.
//
//     chunk ::= block

function parseChunk() {
	next();
	markLocation();
	createScope(true);
	scopeIdentifierName("...");
	const body = parseBlock();
	destroyScope();
	if (EOF !== token.type) unexpected(token);
	// If the body is empty no previousToken exists when finishNode runs.
	if (trackLocations && !body.length) previousToken = token;
	return finishNode(ast.chunk(body));
}

// A block contains a list of statements with an optional return statement
// as its last statement.
//
//     block ::= {stat} [retstat]

function parseBlock() {
	const block = [];
	let statement;

	while (!isBlockFollow(token)) {
		// Return has to be the last statement in a block.
		if ("return" === token.value) {
			block.push(parseStatement());
			break;
		}
		statement = parseStatement();
		consume(";");
		// Statements are only added if they are returned, this allows us to
		// ignore some statements, such as EmptyStatement.
		if (statement) block.push(statement);
	}

	// Doesn't really need an ast node
	return block;
}

// There are two types of statements, simple and compound.
//
//     statement ::= break | goto | do | while | repeat | return
//          | if | for | function | local | label | assignment
//          | functioncall | ';'

function parseStatement() {
	markLocation();
	if (Keyword === token.type) {
		switch (token.value) {
			case "local":
				next();
				return parseLocalStatement();
			case "if":
				next();
				return parseIfStatement();
			case "return":
				next();
				return parseReturnStatement();
			case "function": {
				next();
				const name = parseFunctionName();
				return parseFunctionDeclaration(name);
			}
			case "while":
				next();
				return parseWhileStatement();
			case "for":
				next();
				return parseForStatement();
			case "repeat":
				next();
				return parseRepeatStatement();
			case "break":
				next();
				return parseBreakStatement();
			case "do":
				next();
				return parseDoStatement();
			case "goto":
				next();
				return parseGotoStatement();
		}
	}

	if (
		features.contextualGoto &&
		token.type === Identifier &&
		token.value === "goto" &&
		lookahead.type === Identifier &&
		lookahead.value !== "goto"
	) {
		next();
		return parseGotoStatement();
	}

	if (Punctuator === token.type) {
		if (consume("::")) return parseLabelStatement();
	}
	// Assignments memorizes the location and pushes it manually for wrapper
	// nodes. Additionally empty `;` statements should not mark a location.
	if (trackLocations) locations.pop();

	// When a `;` is encounted, simply eat it without storing it.
	if (features.emptyStatement) {
		if (consume(";")) return;
	}

	return parseAssignmentOrCallStatement();
}

// ## Statements

//     label ::= '::' Name '::'

function parseLabelStatement() {
	const name: any = token.value,
		label = parseIdentifier();

	scopeIdentifierName("::" + name + "::");
	attachScope(label, true);

	expect("::");
	return finishNode(ast.labelStatement(label));
}

//     break ::= 'break'

function parseBreakStatement() {
	return finishNode(ast.breakStatement());
}

//     goto ::= 'goto' Name

function parseGotoStatement() {
	const label = parseIdentifier();

	return finishNode(ast.gotoStatement(label));
}

//     do ::= 'do' block 'end'

function parseDoStatement() {
	createScope(false);
	const body = parseBlock();
	destroyScope();
	expect("end");
	return finishNode(ast.doStatement(body));
}

//     while ::= 'while' exp 'do' block 'end'

function parseWhileStatement() {
	const condition = parseExpectedExpression();
	expect("do");
	createScope(false);
	const body = parseBlock();
	destroyScope();
	expect("end");
	return finishNode(ast.whileStatement(condition, body));
}

//     repeat ::= 'repeat' block 'until' exp

function parseRepeatStatement() {
	createScope(false);
	const body = parseBlock();
	expect("until");
	const condition = parseExpectedExpression();
	destroyScope();
	return finishNode(ast.repeatStatement(condition, body));
}

//     retstat ::= 'return' [exp {',' exp}] [';']

function parseReturnStatement() {
	const expressions = [];

	if ("end" !== token.value) {
		let expression = parseExpression();
		if (null != expression) expressions.push(expression);
		while (consume(",")) {
			expression = parseExpectedExpression();
			expressions.push(expression);
		}
		consume(";"); // grammar tells us ; is optional here.
	}
	return finishNode(ast.returnStatement(expressions));
}

//     if ::= 'if' exp 'then' block {elif} ['else' block] 'end'
//     elif ::= 'elseif' exp 'then' block

function parseIfStatement() {
	const clauses = [];
	let condition;
	let body;
	let marker;

	// IfClauses begin at the same location as the parent IfStatement.
	// It ends at the start of `end`, `else`, or `elseif`.
	if (trackLocations) {
		marker = locations[locations.length - 1];
		locations.push(marker);
	}
	condition = parseExpectedExpression();
	expect("then");
	createScope(false);
	body = parseBlock();
	destroyScope();
	clauses.push(finishNode(ast.ifClause(condition, body)));

	if (trackLocations) marker = createLocationMarker();
	while (consume("elseif")) {
		pushLocation(marker);
		condition = parseExpectedExpression();
		expect("then");
		createScope(false);
		body = parseBlock();
		destroyScope();
		clauses.push(finishNode(ast.elseifClause(condition, body)));
		if (trackLocations) marker = createLocationMarker();
	}

	if (consume("else")) {
		// Include the `else` in the location of ElseClause.
		if (trackLocations) {
			marker = new Marker(previousToken);
			locations.push(marker);
		}
		createScope(false);
		body = parseBlock();
		destroyScope();
		clauses.push(finishNode(ast.elseClause(body)));
	}

	expect("end");
	return finishNode(ast.ifStatement(clauses));
}

// There are two types of for statements, generic and numeric.
//
//     for ::= Name '=' exp ',' exp [',' exp] 'do' block 'end'
//     for ::= namelist 'in' explist 'do' block 'end'
//     namelist ::= Name {',' Name}
//     explist ::= exp {',' exp}

function parseForStatement() {
	let variable = parseIdentifier(),
		body;

	// The start-identifier is local.

	createScope(false);
	scopeIdentifier(variable);

	// If the first expression is followed by a `=` punctuator, this is a
	// Numeric For Statement.
	if (consume("=")) {
		// Start expression
		const start = parseExpectedExpression();
		expect(",");
		// End expression
		const end = parseExpectedExpression();
		// Optional step expression
		const step = consume(",") ? parseExpectedExpression() : null;

		expect("do");
		body = parseBlock();
		expect("end");
		destroyScope();

		return finishNode(
			ast.forNumericStatement(variable, start, end, step, body)
		);
	}
	// If not, it's a Generic For Statement
	else {
		// The namelist can contain one or more identifiers.
		const variables = [variable];
		while (consume(",")) {
			variable = parseIdentifier();
			// Each variable in the namelist is locally scoped.
			scopeIdentifier(variable);
			variables.push(variable);
		}
		expect("in");
		const iterators = [];

		// One or more expressions in the explist.
		do {
			const expression = parseExpectedExpression();
			iterators.push(expression);
		} while (consume(","));

		expect("do");
		body = parseBlock();
		expect("end");
		destroyScope();

		return finishNode(ast.forGenericStatement(variables, iterators, body));
	}
}

// Local statements can either be variable assignments or function
// definitions. If a function definition is found, it will be delegated to
// `parseFunctionDeclaration()` with the isLocal flag.
//
// This AST structure might change into a local assignment with a function
// child.
//
//     local ::= 'local' 'function' Name funcdecl
//        | 'local' Name {',' Name} ['=' exp {',' exp}]

function parseLocalStatement() {
	let name;

	if (Identifier === token.type) {
		const variables = [];
		const init = [];

		do {
			name = parseIdentifier();

			variables.push(name);
		} while (consume(","));

		const types = parseTypeList(true);

		if (consume("=")) {
			do {
				const expression = parseExpectedExpression();
				init.push(expression);
			} while (consume(","));
		}

		// Declarations doesn't exist before the statement has been evaluated.
		// Therefore assignments can't use their declarator. And the identifiers
		// shouldn't be added to the scope until the statement is complete.
		for (let i = 0, l = variables.length; i < l; ++i) {
			scopeIdentifier(variables[i]);
		}

		return finishNode(ast.localStatement(variables, types, init));
	}
	if (consume("function")) {
		name = parseIdentifier();

		scopeIdentifier(name);
		createScope(true);

		// MemberExpressions are not allowed in local function statements.
		return parseFunctionDeclaration(name, true);
	} else {
		raiseUnexpectedToken("<name>", token);
	}
}

function validateVar(node: any) {
	// @TODO we need something not dependent on the exact AST used. see also isCallExpression()
	if (
		["Identifier", "MemberExpression", "IndexExpression"].indexOf(node.type) ===
		-1
	) {
		raise(token, errors.invalidVar, token.value);
	}
}

//     assignment ::= varlist '=' explist
//     let ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
//     varlist ::= let {',' let}
//     explist ::= exp {',' exp}
//
//     call ::= callexp
//     callexp ::= prefixexp args | prefixexp ':' Name args

function parseAssignmentOrCallStatement() {
	// Keep a reference to the previous token for better error messages in case
	// of invalid statement
	const previous = token;
	let marker;

	if (trackLocations) marker = createLocationMarker();
	const expression = parsePrefixExpression();

	if (null == expression) return unexpected(token);
	if (",=".indexOf((token.value: any)) >= 0) {
		const variables = [expression];
		const init = [];
		let exp;

		validateVar(expression);
		while (consume(",")) {
			exp = parsePrefixExpression();
			if (null == exp) raiseUnexpectedToken("<expression>", token);
			validateVar(exp);
			variables.push(exp);
		}
		expect("=");
		do {
			exp = parseExpectedExpression();
			init.push(exp);
		} while (consume(","));

		pushLocation(marker);
		return finishNode(ast.assignmentStatement(variables, init));
	}
	if (isCallExpression(expression)) {
		pushLocation(marker);
		return finishNode(ast.callStatement(expression));
	}
	// The prefix expression was neither part of an assignment or a
	// callstatement, however as it was valid it's been consumed, so raise
	// the exception on the previous token to provide a helpful message.
	return unexpected(previous);
}

// ### Non-statements

function parseFuncTypeArgs() {
	expect("(");
	if (!consume(")")) {
		const typeList = parseTypeList(false);
		expect(")");
		return typeList;
	}
	return ast.typeList([], nil_type);
}

//     functype ::= functypeargs '=>' functypeargs
//     functypeargs ::= '(' [typelist] ')'
function parseFuncType() {
	const parameters = parseFuncTypeArgs();
	expect("=>");
	const returns = parseFuncTypeArgs();
	return finishNode(ast.functionType(parameters, returns));
}

//     tabletype ::= '{' {name ':' typeinfo ','} name ':' typeinfo [','] '}'
//     tabletype ::= '{' '}'
function parseTableType() {
	expect("{");
	const map = new Map();
	while (!consume("}")) {
		const name = parseIdentifier();
		expect(":");
		const type = parseTypeInfo();
		map.set(name.name, type);
		if (!consume(",")) {
			expect("}");
			break;
		}
	}
	return finishNode(ast.tableType(map));
}

function parseTypeInfo() {
	const s = new Set();
	s.add(parseSingleType());
	while (consume("|")) {
		s.add(parseSingleType());
	}
	return finishNode(ast.typeInfo(s));
}

//     singletype ::= 'number' | 'boolean' | 'string' | 'table' | 'function' | 'nil' | 'any' | functype
//     singletype ::= functype
//     singletype ::= tabletype
function parseSingleType() {
	let type;
	if (token.type === Punctuator && token.value === "(") return parseFuncType();
	else if (token.type === Punctuator && token.value === "{")
		return parseTableType();
	else if (token.type === Identifier) type = token.value;
	else if (token.type === NilLiteral) type = "nil";
	else if (token.type === Keyword && token.value === "function")
		type = "function";
	else raiseUnexpectedToken("<type>", token);
	switch (type) {
		case "number":
		case "boolean":
		case "string":
		case "table":
		case "function":
		case "nil":
		case "any":
			next();
			return finishNode(ast.simpleType(type));
		default:
			raiseUnexpectedToken("<type>", token);
	}
}

//     typelist ::= ':' { typeinfo ',' } typeinfo
//     typelist ::=
function parseTypeList(parseColon) {
	if (parseColon && !consume(":"))
		return finishNode(ast.typeList([], any_type));
	const types = [parseTypeInfo()];
	while (consume(",")) types.push(parseTypeInfo());
	return finishNode(ast.typeList(types, nil_type));
}

//     Identifier ::= Name

function parseIdentifier() {
	markLocation();
	const identifier = token.value;
	if (Identifier !== token.type) raiseUnexpectedToken("<name>", token);
	next();
	return finishNode(ast.identifier(identifier));
}

// Parse the functions parameters and body block. The name should already
// have been parsed and passed to this declaration function. By separating
// this we allow for anonymous functions in expressions.
//
// For local functions there's a boolean parameter which needs to be set
// when parsing the declaration.
//
//     funcdecl ::= '(' [parlist] ')' block 'end'
//     parlist ::= Name {',' Name} | [',' '...'] | '...'

function parseFunctionDeclaration(name, isLocal) {
	const parameters = [];
	let parameter_types = null;
	let return_types = null;
	expect("(");
	let has_varargs = false;

	// The declaration has arguments
	if (!consume(")")) {
		// Arguments are a comma separated list of identifiers, optionally ending
		// with a vararg.
		while (true) {
			if (Identifier === token.type) {
				const parameter = parseIdentifier();
				// Function parameters are local.
				scopeIdentifier(parameter);

				parameters.push(parameter);

				if (consume(",")) continue;
				else break;
			}
			// No arguments are allowed after a vararg.
			else if (VarargLiteral === token.type) {
				scopeIdentifierName("...");
				has_varargs = true;
				parsePrimaryExpression(true);
				break;
			} else {
				raiseUnexpectedToken("<name> or '...'", token);
			}
		}
		parameter_types = parseTypeList(true);

		expect(")");
	}

	if (consume(":")) {
		if (token.type === Identifier && token.value === "void") {
			return_types = finishNode(ast.typeList([], nil_type));
			next();
		} else return_types = parseTypeList(false);
	} else return_types = ast.typeList([], any_type);

	const body = parseBlock();
	expect("end");
	destroyScope();

	if (parameter_types == null) parameter_types = ast.typeList([], nil_type);

	return finishNode(
		ast.functionStatement(
			name,
			parameters,
			parameter_types,
			return_types,
			has_varargs,
			isLocal || false,
			body
		)
	);
}

// Parse the function name as identifiers and member expressions.
//
//     Name {'.' Name} [':' Name]

function parseFunctionName() {
	let base, name, marker;

	if (trackLocations) marker = createLocationMarker();
	base = parseIdentifier();

	attachScope(base, scopeHasName(base.name));
	createScope(true);

	while (consume(".")) {
		pushLocation(marker);
		name = parseIdentifier();
		base = finishNode(ast.memberExpression(base, ".", name));
	}

	if (consume(":")) {
		pushLocation(marker);
		name = parseIdentifier();
		base = finishNode(ast.memberExpression(base, ":", name));
		scopeIdentifierName("self");
	}

	return base;
}

//     tableconstructor ::= '{' [fieldlist] '}'
//     fieldlist ::= field {fieldsep field} fieldsep
//     field ::= '[' exp ']' '=' exp | Name = 'exp' | exp
//
//     fieldsep ::= ',' | ';'

function parseTableConstructor() {
	const fields = [];
	let key;
	let value;

	while (true) {
		markLocation();
		if (Punctuator === token.type && consume("[")) {
			key = parseExpectedExpression();
			expect("]");
			expect("=");
			value = parseExpectedExpression();
			fields.push(finishNode(ast.tableKey(key, value)));
		} else if (Identifier === token.type) {
			if ("=" === lookahead.value) {
				key = parseIdentifier();
				next();
				value = parseExpectedExpression();
				fields.push(finishNode(ast.tableKeyString(key, value)));
			} else {
				value = parseExpectedExpression();
				fields.push(finishNode(ast.tableValue(value)));
			}
		} else {
			if (null == (value = parseExpression())) {
				locations.pop();
				break;
			}
			fields.push(finishNode(ast.tableValue(value)));
		}
		if (",;".indexOf((token.value: any)) >= 0) {
			next();
			continue;
		}
		break;
	}
	expect("}");
	return finishNode(ast.tableConstructorExpression(fields));
}

// Expression parser
// -----------------
//
// Expressions are evaluated and always return a value. If nothing is
// matched null will be returned.
//
//     exp ::= (unop exp | primary | prefixexp ) { binop exp }
//
//     primary ::= nil | false | true | Number | String | '...'
//          | functiondef | tableconstructor
//
//     prefixexp ::= (Name | '(' exp ')' ) { '[' exp ']'
//          | '.' Name | ':' Name args | args }
//

function parseExpression() {
	const expression = parseSubExpression(0);
	return expression;
}

// Parse an expression expecting it to be valid.

function parseExpectedExpression() {
	const expression = parseExpression();
	if (null == expression) raiseUnexpectedToken("<expression>", token);
	else return expression;
}

// Return the precedence priority of the operator.
//
// As unary `-` can't be distinguished from binary `-`, unary precedence
// isn't described in this table but in `parseSubExpression()` itself.
//
// As this function gets hit on every expression it's been optimized due to
// the expensive CompareICStub which took ~8% of the parse time.

function binaryPrecedence(operator: any) {
	const charCode = operator.charCodeAt(0),
		length = operator.length;

	if (1 === length) {
		switch (charCode) {
			case 94:
				return 12; // ^
			case 42:
			case 47:
			case 37:
				return 10; // * / %
			case 43:
			case 45:
				return 9; // + -
			case 38:
				return 6; // &
			case 126:
				return 5; // ~
			case 124:
				return 4; // |
			case 60:
			case 62:
				return 3; // < >
		}
	} else if (2 === length) {
		switch (charCode) {
			case 47:
				return 10; // //
			case 46:
				return 8; // ..
			case 60:
			case 62:
				if ("<<" === operator || ">>" === operator) return 7; // << >>
				return 3; // <= >=
			case 61:
			case 126:
				return 3; // == ~=
			case 111:
				return 1; // or
		}
	} else if (97 === charCode && "and" === operator) return 2;
	return 0;
}

// Implement an operator-precedence parser to handle binary operator
// precedence.
//
// We use this algorithm because it's compact, it's fast and Lua core uses
// the same so we can be sure our expressions are parsed in the same manner
// without excessive amounts of tests.
//
//     exp ::= (unop exp | primary | prefixexp ) { binop exp }

function parseSubExpression(minPrecedence) {
	let operator = token.value,
		// The left-hand side in binary operations.
		expression,
		marker;

	if (trackLocations) marker = createLocationMarker();

	// UnaryExpression
	if (isUnary(token)) {
		markLocation();
		next();
		const argument = parseSubExpression(10);
		if (argument == null) raiseUnexpectedToken("<expression>", token);
		expression = finishNode(ast.unaryExpression(operator, argument));
	}
	if (null == expression) {
		// PrimaryExpression
		expression = parsePrimaryExpression();

		// PrefixExpression
		if (null == expression) {
			expression = parsePrefixExpression();
		}
	}
	// This is not a valid left hand expression.
	if (null == expression) return null;

	let precedence;
	while (true) {
		operator = token.value;

		precedence =
			Punctuator === token.type || Keyword === token.type
				? binaryPrecedence(operator)
				: 0;

		if (precedence === 0 || precedence <= minPrecedence) break;
		// Right-hand precedence operators
		if ("^" === operator || ".." === operator) precedence--;
		next();
		const right = parseSubExpression(precedence);
		if (null == right) raiseUnexpectedToken("<expression>", token);
		// Push in the marker created before the loop to wrap its entirety.
		if (trackLocations) locations.push(marker);
		expression = finishNode(ast.binaryExpression(operator, expression, right));
	}
	return expression;
}

//     prefixexp ::= prefix {suffix}
//     prefix ::= Name | '(' exp ')'
//     suffix ::= '[' exp ']' | '.' Name | ':' Name args | args
//
//     args ::= '(' [explist] ')' | tableconstructor | String

function parsePrefixExpression() {
	let base, name, marker;

	if (trackLocations) marker = createLocationMarker();

	// The prefix
	if (Identifier === token.type) {
		name = token.value;
		base = parseIdentifier();
		// Set the parent scope.
		attachScope(base, scopeHasName(name));
	} else if (consume("(")) {
		base = parseExpectedExpression();
		expect(")");
		base = finishNode(ast.parenthesisExpression(base));
	} else {
		return null;
	}

	// The suffix
	let expression, identifier;
	while (true) {
		if (Punctuator === token.type) {
			switch (token.value) {
				case "[":
					pushLocation(marker);
					next();
					expression = parseExpectedExpression();
					expect("]");
					base = finishNode(ast.indexExpression(base, expression));
					break;
				case ".":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					base = finishNode(ast.memberExpression(base, ".", identifier));
					break;
				case ":":
					pushLocation(marker);
					next();
					identifier = parseIdentifier();
					base = finishNode(ast.memberExpression(base, ":", identifier));
					// Once a : is found, this has to be a CallExpression, otherwise
					// throw an error.
					pushLocation(marker);
					base = parseCallExpression(base);
					break;
				case "(":
				case "{": // args
					pushLocation(marker);
					base = parseCallExpression(base);
					break;
				default:
					return base;
			}
		} else if (StringLiteral === token.type) {
			pushLocation(marker);
			base = parseCallExpression(base);
		} else {
			break;
		}
	}

	return base;
}

//     args ::= '(' [explist] ')' | tableconstructor | String

function parseCallExpression(base) {
	if (Punctuator === token.type) {
		switch (token.value) {
			case "(": {
				if (!features.emptyStatement) {
					if (token.line !== previousToken.line)
						raise({}, errors.ambiguousSyntax, token.value);
				}
				next();

				// List of expressions
				const expressions = [];
				let expression = parseExpression();
				if (null != expression) expressions.push(expression);
				while (consume(",")) {
					expression = parseExpectedExpression();
					expressions.push(expression);
				}

				expect(")");
				return finishNode(ast.callExpression(base, expressions));
			}
			case "{": {
				markLocation();
				next();
				const table = parseTableConstructor();
				return finishNode(ast.tableCallExpression(base, table));
			}
		}
	} else if (StringLiteral === token.type) {
		return finishNode(ast.stringCallExpression(base, parsePrimaryExpression()));
	}

	raiseUnexpectedToken("function arguments", token);
}

//     primary ::= String | Numeric | nil | true | false
//          | functiondef | tableconstructor | '...'
// if identifier is true, it is parsing an identifier so does not
// check for error with '...'
function parsePrimaryExpression(identifier) {
	const literals =
		StringLiteral |
		NumericLiteral |
		BooleanLiteral |
		NilLiteral |
		VarargLiteral;
	const value = token.value;
	const type = token.type;
	let marker;

	if (trackLocations) marker = createLocationMarker();

	if (type & literals) {
		if (!identifier && type === VarargLiteral && !scopeHasName("..."))
			raise(token, "cannot use '...' outside a vararg function");
		pushLocation(marker);
		const raw = input.slice(token.range[0], token.range[1]);
		next();
		return finishNode(ast.literal(type, value, raw));
	} else if (Keyword === type && "function" === value) {
		pushLocation(marker);
		next();
		createScope(true);
		return parseFunctionDeclaration(null);
	} else if (consume("{")) {
		pushLocation(marker);
		return parseTableConstructor();
	}
}

// Parser
// ------

// Export the main parser.
//
//   - `wait` Hold parsing until end() is called. Defaults to false
//   - `comments` Store comments. Defaults to true.
//   - `scope` Track identifier scope. Defaults to false.
//   - `locations` Store location information. Defaults to false.
//   - `ranges` Store the start and end character locations. Defaults to
//     false.
//   - `onCreateNode` Callback which will be invoked when a syntax node is
//     created.
//
// Example:
//
//     let parser = require('luaparser');
//     parser.parse('i = 0');

const versionFeatures = {
	"5.1": {},
	"5.2": {
		labels: true,
		emptyStatement: true,
		hexEscapes: true,
		skipWhitespaceEscape: true,
		strictEscapes: true,
	},
	"5.3": {
		labels: true,
		emptyStatement: true,
		hexEscapes: true,
		skipWhitespaceEscape: true,
		strictEscapes: true,
		unicodeEscapes: true,
		bitwiseOperators: true,
		integerDivision: true,
	},
	LuaJIT: {
		// XXX: LuaJIT language features may depend on compilation options; may need to
		// rethink how to handle this. Specifically, there is a LUAJIT_ENABLE_LUA52COMPAT
		// that removes contextual goto. Maybe add 'LuaJIT-5.2compat' as well?
		labels: true,
		contextualGoto: true,
		hexEscapes: true,
		skipWhitespaceEscape: true,
		strictEscapes: true,
		unicodeEscapes: true,
	},
};

function parse(_input: any, _options: any): any {
	if ("undefined" === typeof _options && "object" === typeof _input) {
		// $FlowFixMe
		_options = _input; // eslint-disable-line no-param-reassign
		// $FlowFixMe
		_input = undefined; // eslint-disable-line no-param-reassign
	}
	// $FlowFixMe
	if (!_options) _options = {}; // eslint-disable-line no-param-reassign

	input = _input || "";
	options = extend(defaultOptions, _options);

	// Rewind the lexer
	index = 0;
	line = 1;
	lineStart = 0;
	length = input.length;
	// When tracking identifier scope, initialize with an empty scope.
	scopes = [];
	function_scope = [];
	scopeDepth = -1;
	globals = [];
	locations = [];

	if (!(features = versionFeatures[options.luaVersion])) {
		throw new Error(
			sprintf("Lua version '%1' not supported", options.luaVersion)
		);
	}

	if (options.comments) comments = [];
	if (!options.wait) return end();
}

function write(_input) {
	input += String(_input);
	length = input.length;
}

function end(_input) {
	if ("undefined" !== typeof _input) write(_input);

	// Ignore shebangs.
	if (input && input.substr(0, 2) === "#!")
		input = input.replace(/^.*/, function(line) {
			return line.replace(/./g, " ");
		});

	length = input.length;
	trackLocations = options.locations || options.ranges;
	// Initialize with a lookahead token.
	lookahead = lex();

	const chunk = parseChunk();
	if (options.comments) chunk.comments = comments;
	chunk.globals = globals;

	if (locations.length > 0)
		throw new Error(
			"Location tracking failed. This is most likely a bug in luaparse"
		);

	return chunk;
}

// Fixing types, for now
const ast_: {
	simpleType: string => AST.SimpleType,
	functionType: (AST.TypeList, AST.TypeList) => AST.FunctionType,
	typeList: (Array<AST.TypeInfo>, AST.TypeInfo) => AST.TypeList,
	tableType: (Map<string, AST.TypeInfo>) => AST.TableType,
	typeInfo: (Set<AST.SingleType>) => AST.TypeInfo,
} = ast;

const parse_: (string, LuaParseOptions) => AST.Chunk = parse;

export { ast_ as ast, parse_ as parse };
