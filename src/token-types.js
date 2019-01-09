// @flow strict

export type LocationInfo = {|
	line: number,
	lineStart: number,
	range: [number, number],
|};

// This actually is never returned by lex, but it is used internally
export type Placeholder = {|
	+type: 1,
	+value: string,
	...LocationInfo,
|};

export type StringLiteral = {|
	+type: 2,
	+value: string,
	...LocationInfo,
	// strings may be multi-line
	lastLine: number,
	lastLineStart: number,
|};

export type Keyword = {|
	+type: 4,
	+value: string,
	...LocationInfo,
|};

export type Identifier = {|
	+type: 8,
	+value: string,
	...LocationInfo,
|};

export type NumericLiteral = {|
	+type: 16,
	+value: number,
	...LocationInfo,
|};

export type Punctuator = {|
	+type: 32,
	+value: string,
	...LocationInfo,
|};

export type BooleanLiteral = {|
	+type: 64,
	+value: boolean,
	...LocationInfo,
|};

export type NilLiteral = {|
	+type: 128,
	+value: null,
	...LocationInfo,
|};

export type VarargLiteral = {|
	+type: 256,
	+value: "...",
	...LocationInfo,
|};

export type Any =
	| StringLiteral
	| Keyword
	| Identifier
	| NumericLiteral
	| Punctuator
	| BooleanLiteral
	| NilLiteral
	| VarargLiteral;
