// @flow strict-local

import * as Token from "./token-types";

import { errors, raise, unexpected } from "./errors";

import type { Comment } from "./ast-types";

// The available tokens expressed as enum flags so they can be checked with
// bitwise operations.

const StringLiteral = 2;
const Keyword = 4;
const Identifier = 8;
const NumericLiteral = 16;
const Punctuator = 32;
const BooleanLiteral = 64;
const NilLiteral = 128;
const VarargLiteral = 256;

export type TokenizerOptions = {|
	// if present, comments are added to this array as they are read
	comments?: Array<Comment>,
	extendedIdentifiers?: boolean,
	luaVersion?: "5.1" | "5.2" | "5.3" | "LuaJIT",
	ignoreShebang?: boolean,
|};

// These are only the features useful for tokenizing
const versionFeatures = {
	"5.1": {},
	"5.2": {
		labels: true,
		hexEscapes: true,
		skipWhitespaceEscape: true,
		strictEscapes: true,
	},
	"5.3": {
		labels: true,
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
	comments: undefined,
	luaVersion: "5.1",
	extendedIdentifiers: false,
	ignoreShebang: true,
};

export function* tokenize(
	input_: string,
	options_: TokenizerOptions
): Generator<Token.Any, void, void> {
	let index = 0;
	let line = 1;
	let lineStart = 0;
	const input = input_;
	let tokenStart;
	const options = Object.assign({}, defaultOptions, options_);
	const features = versionFeatures[options.luaVersion];

	// Ignore shebangs.
	if (options.ignoreShebang && input.substr(0, 2) === "#!")
		while (!consumeEOL()) index++;

	while (true) {
		const token = lex();
		if (token != null) yield token;
		else return;
	}
	// Below are the functions used by this closure

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

	function lex(): ?Token.Any {
		skipWhiteSpace();

		// Skip comments beginning with --
		while (
			45 === input.charCodeAt(index) &&
			45 === input.charCodeAt(index + 1)
		) {
			scanComment();
			skipWhiteSpace();
		}
		if (index >= input.length) return null;

		const charCode = input.charCodeAt(index);
		const next = input.charCodeAt(index + 1);

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

		throw unexpected(input.charAt(index));
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
		while (index < input.length) {
			const charCode = input.charCodeAt(index);
			if (isWhiteSpace(charCode)) {
				++index;
			} else if (!consumeEOL()) {
				break;
			}
		}
	}

	function encodeUTF8(codepoint: number): string {
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
			throw new Error(`Invalid codepoint ${codepoint}`);
		}
	}

	// This function takes a JavaScript string, encodes it in WTF-8 and
	// reinterprets the resulting code units as code points; i.e. it encodes
	// the string in what was the original meaning of WTF-8.
	//
	// For a detailed rationale, see the README.md file, section
	// "Note on character encodings".

	function fixupHighCharacters(s: string): string {
		// eslint-disable-next-line no-control-regex
		return s.replace(/[\ud800-\udbff][\udc00-\udfff]|[^\x00-\x7f]/g, function(
			m
		) {
			if (m.length === 1) return encodeUTF8(m.charCodeAt(0));
			return encodeUTF8(
				0x10000 +
					(((m.charCodeAt(0) & 0x3ff) << 10) | (m.charCodeAt(1) & 0x3ff))
			);
		});
	}

	// Identifiers, keywords, booleans and nil all look the same syntax wise. We
	// simply go through them one by one and defaulting to an identifier if no
	// previous case matched.

	function scanIdentifierOrKeyword():
		| Token.Identifier
		| Token.Keyword
		| Token.BooleanLiteral
		| Token.NilLiteral {
		// Slicing the input string is prefered before string concatenation in a
		// loop for performance reasons.
		while (isIdentifierPart(input.charCodeAt(++index)));
		const value = fixupHighCharacters(input.slice(tokenStart, index));

		const loc = { line, lineStart, range: [tokenStart, index] };

		// Decide on the token type and possibly cast the value.
		if (isKeyword(value)) {
			return {
				type: Keyword,
				value,
				...loc,
			};
		} else if ("true" === value || "false" === value) {
			return {
				type: BooleanLiteral,
				value: value === "true",
				...loc,
			};
		} else if ("nil" === value) {
			return {
				type: NilLiteral,
				value: null,
				...loc,
			};
		} else {
			return {
				type: Identifier,
				value,
				...loc,
			};
		}
	}

	// Once a punctuator reaches this function it should already have been
	// validated so we simply return it as a token.

	function scanPunctuator(value): Token.Punctuator {
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

	function scanVarargLiteral(): Token.VarargLiteral {
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

	function scanStringLiteral(): Token.StringLiteral {
		const delimiter = input.charCodeAt(index++);
		const beginLine = line;
		const beginLineStart = lineStart;
		let stringStart = index;
		let string = "";
		let charCode;

		while (index < input.length) {
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
			// ending delimiter by now, throw raise an exception.
			if (index >= input.length || isLineTerminator(charCode)) {
				string += input.slice(stringStart, index - 1);
				throw raise(
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

	function scanLongStringLiteral(): Token.StringLiteral {
		const beginLine = line,
			beginLineStart = lineStart,
			string = readLongString(false);
		// Fail if it's not a multiline literal.
		if (null == string) throw raise({}, errors.expected, "[", "?");

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

	function scanNumericLiteral(): Token.NumericLiteral {
		const character = input.charAt(index);
		const next = input.charAt(index + 1);

		const value =
			"0" === character && "xX".indexOf(next || " ") >= 0
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
			throw raise({}, errors.malformedNumber, input.slice(tokenStart, index));

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
		if ("pP".indexOf(input.charAt(index) || " ") >= 0) {
			++index;

			// Sign part is optional and defaults to 1 (positive).
			if ("+-".indexOf(input.charAt(index) || " ") >= 0)
				binarySign = "+" === input.charAt(index++) ? 1 : -1;

			exponentStart = index;

			// The binary exponent sign requires a decimal digit.
			if (!isDecDigit(input.charCodeAt(index)))
				throw raise({}, errors.malformedNumber, input.slice(tokenStart, index));

			while (isDecDigit(input.charCodeAt(index))) ++index;
			binaryExponent = parseInt(input.slice(exponentStart, index));

			// Calculate the binary exponent of the number.
			binaryExponent = Math.pow(2, binaryExponent * binarySign);
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
		if ("eE".indexOf(input.charAt(index) || " ") >= 0) {
			++index;
			// Sign part is optional.
			if ("+-".indexOf(input.charAt(index) || " ") >= 0) ++index;
			// An exponent is required to contain at least one decimal digit.
			if (!isDecDigit(input.charCodeAt(index)))
				throw raise({}, errors.malformedNumber, input.slice(tokenStart, index));

			while (isDecDigit(input.charCodeAt(index))) ++index;
		}

		return parseFloat(input.slice(tokenStart, index));
	}

	function readUnicodeEscapeSequence() {
		const sequenceStart = index++;

		if (input.charAt(index++) !== "{")
			throw raise(
				{},
				errors.braceExpected,
				"{",
				"\\" + input.slice(sequenceStart, index)
			);
		if (!isHexDigit(input.charCodeAt(index)))
			throw raise(
				{},
				errors.hexadecimalDigitExpected,
				"\\" + input.slice(sequenceStart, index)
			);

		while (input.charCodeAt(index) === 0x30) ++index;
		const escStart = index;

		while (isHexDigit(input.charCodeAt(index))) {
			++index;
			if (index - escStart > 6)
				throw raise(
					{},
					errors.tooLargeCodepoint,
					"\\" + input.slice(sequenceStart, index)
				);
		}

		const b = input.charAt(index++);
		if (b !== "}") {
			if (b === '"' || b === "'")
				throw raise(
					{},
					errors.braceExpected,
					"}",
					"\\" + input.slice(sequenceStart, index--)
				);
			else
				throw raise(
					{},
					errors.hexadecimalDigitExpected,
					"\\" + input.slice(sequenceStart, index)
				);
		}

		let codepoint = parseInt(input.slice(escStart, index - 1), 16);

		codepoint = encodeUTF8(codepoint);
		if (codepoint === null) {
			throw raise(
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
					throw raise({}, errors.decimalEscapeTooLarge, "\\" + ddd);
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
					throw raise(
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
					throw raise(
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
			if (null == content) content = character;
			else isLong = true;
		}
		// Scan until next line as long as it's not a multiline comment.
		if (!isLong) {
			while (index < input.length) {
				if (isLineTerminator(input.charCodeAt(index))) break;
				++index;
			}
			if (options.comments === true) content = input.slice(commentStart, index);
		}

		const lineStartComment = lineStart;
		const lineComment = line;
		if (options.comments) {
			options.comments.push({
				type: "Comment",
				value: content,
				raw: input.slice(tokenStart, index),
				loc: {
					start: { line: lineComment, column: tokenStart - lineStartComment },
					end: { line, column: index - lineStart },
				},
				range: [tokenStart, index],
			});
		}
	}

	// Read a multiline string by calculating the depth of `=` characters and
	// then appending until an equal depth is found.

	function readLongString(isComment: boolean): ?string {
		let level = 0;
		let content = "";
		let terminator = false;
		let character;
		const firstLine = line;

		++index; // [

		// Calculate the depth of the comment.
		while ("=" === input.charAt(index + level)) ++level;
		// Exit, this is not a long string afterall.
		if ("[" !== input.charAt(index + level)) return null;

		index += level + 1;

		// If the first character is a newline, ignore it and begin on next line.
		if (isLineTerminator(input.charCodeAt(index))) consumeEOL();

		const stringStart = index;
		while (index < input.length) {
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

		throw raise(
			{},
			isComment ? errors.unfinishedLongComment : errors.unfinishedLongString,
			firstLine,
			"<eof>"
		);
	}

	// ### Validation functions

	function isWhiteSpace(charCode: number): boolean {
		return (
			9 === charCode || 32 === charCode || 0xb === charCode || 0xc === charCode
		);
	}

	function isLineTerminator(charCode: number): boolean {
		return 10 === charCode || 13 === charCode;
	}

	function isDecDigit(charCode: number): boolean {
		return charCode >= 48 && charCode <= 57;
	}

	function isHexDigit(charCode: number): boolean {
		return (
			(charCode >= 48 && charCode <= 57) ||
			(charCode >= 97 && charCode <= 102) ||
			(charCode >= 65 && charCode <= 70)
		);
	}

	// From [Lua 5.2](http://www.lua.org/manual/5.2/manual.html#8.1) onwards
	// identifiers cannot use 'locale-dependent' letters (i.e. dependent on the C locale).
	// On the other hand, LuaJIT allows arbitrary octets ≥ 128 in identifiers.

	function isIdentifierStart(charCode: number): boolean {
		if (
			(charCode >= 65 && charCode <= 90) ||
			(charCode >= 97 && charCode <= 122) ||
			95 === charCode
		)
			return true;
		if (options.extendedIdentifiers === true && charCode >= 128) return true;
		return false;
	}

	function isIdentifierPart(charCode: number): boolean {
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

	function isKeyword(id: string): boolean {
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
}
