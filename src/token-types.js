// @flow strict

export type StringLiteral = {
	+type: 2,
	+value: string,
};

export type Keyword = {
	+type: 4,
	+value: string,
};

export type Identifier = {
	+type: 8,
	+value: string,
};

export type NumericLiteral = {
	+type: 16,
	+value: number,
};

export type Punctuator = {
	+type: 32,
	+value: string,
};

export type BooleanLiteral = {
	+type: 64,
	+value: boolean,
};

export type NilLiteral = {
	+type: 128,
	+value: null,
};

export type VarargLiteral = {
	+type: 256,
	+value: "...",
};

export type Any =
	| StringLiteral
	| Keyword
	| Identifier
	| NumericLiteral
	| Punctuator
	| BooleanLiteral
	| NilLiteral
	| VarargLiteral;
