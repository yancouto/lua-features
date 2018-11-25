// @flow

let index;
let input;
let length;
let line;
let lineStart;
let features;
let tokenStart;
let token;
let options;
let lookahead;

export type TokenizerOptions = {|
	comments?: boolean,
	extendedIdentifiers?: boolean,
	luaVersion?: "5.1" | "5.2" | "5.3" | "LuaJIT",
|};

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

const defaultOptions = {
	comments: true,
	luaVersion: "5.1",
	extendedIdentifiers: false,
};

export function* tokenize(input_: string, options_: TokenizerOptions): any {
	index = 0;
	line = 1;
	lineStart = 0;
	input = input_;
	// Ignore shebangs.
	if (input.substr(0, 2) === "#!")
		input = input.replace(/^.*/, function(line) {
			return line.replace(/./g, " ");
		});
	length = input.length;
	options = Object.assign({}, defaultOptions, options_);
	if (!(features = versionFeatures[options.luaVersion])) {
		throw new Error(
			sprintf("Lua version '%1' not supported", options.luaVersion)
		);
	}

	while (true) {
		const x = lex();
		if (x.type !== EOF) yield x;
		else return;
	}
}

// The available tokens expressed as enum flags so they can be checked with
// bitwise operations.

const EOF = 1;
const StringLiteral = 2;
const Keyword = 4;
const Identifier = 8;
const NumericLiteral = 16;
const Punctuator = 32;
const BooleanLiteral = 64;
const NilLiteral = 128;
const VarargLiteral = 256;

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
			line,
			lineStart,
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
		type,
		value,
		line,
		lineStart,
		range: [tokenStart, index],
	};
}

// Once a punctuator reaches this function it should already have been
// validated so we simply return it as a token.

function scanPunctuator(value) {
	index += value.length;
	return {
		type: Punctuator,
		value,
		line,
		lineStart,
		range: [tokenStart, index],
	};
}

// A vararg literal consists of three dots.

function scanVarargLiteral() {
	index += 3;
	return {
		type: VarargLiteral,
		value: "...",
		line,
		lineStart,
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
		value,
		line,
		lineStart,
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
		if (options.comments === true) content = input.slice(commentStart, index);
	}

	// TODO uncomment this
	//const lineStartComment = lineStart;
	//const lineComment = line;
	//if (options.comments) {
	//	const node: any = ast.comment(content, input.slice(tokenStart, index));

	//	// `Marker`s depend on tokens available in the parser and as comments are
	//	// intercepted in the lexer all location data is set manually.
	//	if (options.locations) {
	//		node.loc = {
	//			start: { line: lineComment, column: tokenStart - lineStartComment },
	//			end: { line, column: index - lineStart },
	//		};
	//	}
	//	if (options.ranges) {
	//		node.range = [tokenStart, index];
	//	}
	//	comments.push(node);
	//}
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
	if (options.extendedIdentifiers === true && charCode >= 128) return true;
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
	if (options.extendedIdentifiers === true && charCode >= 128) return true;
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

export function unexpected(found: any) {
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