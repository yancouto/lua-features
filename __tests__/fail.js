// @flow strict-local
/* eslint-env jest */
import { check } from "../src/lua-type-check";

const assignments = [
	"a",
	"a,",
	"a,b,c",
	"a,b =",
	"0 = 0",
	'"foo" = 0',
	"true = 0",
	"(a) = 0",
	"{} = 0",
	"a:b() = 0",
	"a() = 0",
	"a.b:c() = 0",
	"a[b]() = 0",
	"a = a b",
	"a = 1 2",
	"a = a = 1",
];

const comments = ["--[=[x]x", "--[[\nbreak", "--[==[oi]]"];

const conditionals = [
	"if",
	"elseif",
	"else",
	"then",
	"if then",
	"if true",
	"if true then",
	"if true else",
	"if true then else",
	"if true then elseif",
	"if true then elseif 2",
	"if true then elseif 2 then",
	"if true then else if 2 then end",
	"if true then return return end",
	"if then end",
	"if true then elseif then end",
];

const do_ = [
	"do",
	"end",
	"do 1 end",
	'do "foo" end',
	"do end do",
	"do end end",
	"do return return end",
];

const escapesequences = [
	'a = "bar\nbaz"',
	'a = "bar\rbaz"',
	"a = '\\255",
	"a = '\\256'",
	"a = '\\300'",
];

const expressions = [
	"a =",
	"a = a.b.",
	"a = a:b",
	"a = a[]",
	"a = ()",
	"a = function",
	"a = function 1",
	"a = function a",
	"a = function end",
	"a = function(",
	"a = function(1",
	"a = function(p,)",
	"a = function(p q",
	"a = function(p,q,1",
	"a = function(...,",
	"a = function(..., p) end",
];

const for_ = [
	"for",
	"for do",
	"for end",
	"for 1",
	"for a",
	"for true",
	"for a, in",
	"for a in",
	"for a do",
	"for a in do",
	"for a in b do",
	"for a in b end",
	"for a in b, do",
	"for a in b do 1 end",
	'for a in b do "foo" end',
	"for a b in",
	"for a in b do return return end",
	"for =",
	"for a =",
	"for a, b =",
	"for a = do",
	"for a = 1, do",
	"for a = p, q, do",
	"for a = p q do",
	"for a = b do end",
	"for a = 1, 2, 3, 4 do end",
	"for a = 1, 2 do 3 end",
	'for a = 1, 2 do "foo" end',
	"for a = 1, 2 do return return end",
];

const functioncalls = [
	"a(",
	"a(1,)",
	"1()",
	"a.1",
	"a.b",
	"a[b]",
	"a.b.(",
	"a:b",
	"a:1",
	"a:b:",
	"()()",
	"a\n()",
	"a.b\n()",
	"a:b\n()",
	"(a\n())",
	"(a)\n()",
	"(a\n())",
	'(a\n"")',
	"(a\n{})",
];

const functions = [
	"function",
	"function 1",
	"function end",
	"function a",
	"function a end",
	"function a( end",
	"function a(1",
	'function a("foo"',
	"function a(p",
	"function a(p,)",
	"function a(p q",
	"function a(p,q,) end",
	"function a(p,q,1",
	"function a(p) do",
	"function a(p) 1 end",
	"function a(p) return return end",
	"function a.(",
	"function a.1",
	"function a.b,",
	"function a.b.(",
	"function a:",
	"function a:1",
	"function a:b:",
	"function a:b.",
	"function a(...,",
	"function a(...,p) end",
];

const literals = [
	"a",
	"a = 1e",
	"a = 0x",
	"a = 0xfp",
	'a = "bar',
	"a = [=aa",
	"a = [=[]",
	"a = [=[x]",
	"a = [=[x]x",
];

const operators = [
	"a = -",
	"a = not",
	"a = 1 +",
	"a = 1 ..",
	"a = 1 * /",
	"a = 1 * -",
	"a = 1 / not",
	"a = ((1",
	"a = ((1 + 2)",
	"a = 1)",
	"a = 1 ==",
	"a = `",
	"a = ~",
	"a = ~= 2",
	"a = 1 and",
	"a = or 1",
];

const repeat = [
	"repeat",
	"repeat until",
	"repeat until local",
	"repeat end",
	"repeat 1",
	"repeat =",
	"repeat 2 until true",
	'repeat "foo" until true',
	"repeat return return until false",
];

const return_ = [
	"function f() return return end",
	"function f() return local end",
	"function f() return 1, end",
	"function f(): number return end",
];

const statements = ["nil", ";", "goto foo"];

const tableconstructors = [
	"a = {",
	"a = {,}",
	"a = {;}",
	"a = {,,}",
	"a = {;;}",
	"a = {{",
	"a = { a",
	"a = { a=",
	"a = { a=,",
	"a = { a=;",
	'a = { 1, a="foo"',
	"a = { [",
	"a = { [1",
	"a = { [1]",
	"a = { [a]=",
	"a = { [] }",
	"a = { a= }",
	"a = { a.b=1 }",
	"a = { a[b[c]]=1 }",
	"a = { 2 3 4 }",
];

const while_ = [
	"while",
	"while do",
	"while =",
	"while true do",
	"while true do 2 end",
	'while true do "foo" end',
	"while true do while",
	"while true end",
	"while true 2 do",
	"while true = 2 do",
	"while true do return return end",
];

const extra = [
	"function a(p q) end",
	"function a(...) return function() print(...) end end",
	"... = 1",
	"a['oi']:get() = 1",
];

const types = [
	"local a = 12 : number",
	"function(a : number, b) end",
	"local function(a: number, b) end",
	// type error
	'local a : number = "oi"',
	"local a, b : number, boolean = 1, 1",
	"local a : number = 1; local b : string = a",
	"local x : number = 1 > 2",
	'local x = "oi" > 2',
	'local x = "1" < 2',
	'local x = "1" == 1;',
	'local s = "oi" .. 1',
	"local x = #1",
	'local x = -"oi"',
	'local x = ~"oi"',
	"local x = 1 + '1'",
	//"local x : number = y", // XXX should these be allowed?
	//"local x : number = f()",
	"while 1 do end",
	"local x : string = 'a'; while x do end",
	"while true do local x : number = '1' end",
	"local x : number = 1; while false do local x : string = 'a' end; local z = x .. 'b'",
	"local x : number = 1; repeat local x : string = 'a' until false; local z = x .. 'b'",
	"local x : number = 1; x = '1'",
	"local x : table = {a = true .. 'a'}",
	"local x : table = {['oi'] = true .. 'a'}",
	"local x : table = {true .. 'a'}",
	"a[1 + 'a'] = 1",
	"(1 + 'a').x = 1",
	"local x : number = 1; x.a = 2;",
	"(function(a : string) a = 1 end)()",
	"function f(a: string) a = 1 end",
	"local x : number = 1; (function(x : string) end)(); x = 'a'",
	"function a[1 + 'a'] () end",
	"local f : number = 1; local function f() end; f = 3",
	"local a : number = 1; do local a = 1; function a() end; end; a()",
	"local f : number = 1; function f() end",
	"return 1 + 'a'",
	"local x : number = 1; x.a()",
	"a.b(1 + 'a')",
	"if true then a = 1 + 'a' end",
	"if 1 then end",
	"for i = 1, 2 do a = 'a' + 1 end",
	"for i = 1, '2' do end",
	"for i = 1, 2 do i = 'a' end",
	"a = 1 + 'oi'",
	"local a = 1, 2, 'a' + 1",
	"a = 1, 2, 'a' + 1",
	"function f(... : number) local a : string = ... end",
	"function f(... : number, string, string) local a, x : number, number = ... end",
	"function f(... : number, string, string) local a, x : number, string = (...) end",
	"(function(... : string, number) end)()",
	"(function(... : string, number) end)('a')",
	"(function(): string; return 1; end)()",
	// TODO make this break. Should see all the branches and assume they return
	// nothing unless there is a return statement
	//"(function(): number if a() then else return 1 end end)()",
	//"local function f(): number end",
	//"local function f(): number | string end",
	"function f(): void return 1 end",
	"function a:f(): number return self end",
	"function a:f() self = 12 end",
	// TODO these will only work when I get tables and "global vars" working
	//"function f(a, b : number, number) end \n f(1, 2, 3)",
	//"function f(a, b : number, number) end \n f()",
	"local function f(a, b : number, number) end \n f(1, 2, 3)",
	"local function f(a, b : number, number) end \n f()",
	"local f : (number) => (string) = function(a: number): string return 'a' end\nlocal x : number = f(1)",
	"local f : (() => ()) => (() => ()) = function(a: () => ()): () => () return a end\n f(f)",
	"local f : (number) => (string) = function(): string return 'a' end",
	"local f : () => (string) = function(): void return end",
	"local function f(): number return 1 end\nlocal function g(): void return f() end",
	"local function f(...: number, string) local a, b, c : number, string, number = ..., 1 end",
	"local function f(): number, string return 1, 'a' end\nlocal a, b, c, d : number, string, number, string = f(), f()",
	"local function f(... :number, number) local a, b : number, nil = ... end",
	// TODO these will break when multiple return values work properly. Maybe add an 'empty' type? Or maybe this should work?
	// "local function f(): void end\nlocal function g(): nil return f() end",
	"local x : {oi: number} = {oi = 1}\nx.oi = 'a'",
	"local x : {oi: number} = {oi = 1}\nx.tchau = 10",
	"local function f(): {a: number} return {a = 1} end\nlocal x: number = f().b",
	"local function f(): {a: number} return {a = 1} end\nlocal x: string = f().a",
	"local y : {a: number, b: string} = {a = 1, b = 'a'}\nlocal function f(a, b : string, number): void end\nf(y.a, y.b)",
	"local y : {a: number, b: string} = {a = 1, b = 1}",
	"local x : number | string = true",
	"local x : number | string = 1; local y : number = x;",
	"local function f(a: number | string): void end; f{}",
	"local function f(a: number): void end; local x : number | string = 1; f(x)",
	"local function f(): number | string; return 1; end; local x : number = f()",
	"local function f(a: () => (string) | () => (number)) local x : number = a() end",
	"local function f(a: (number) => (number) | (string) => (string)); a(1) end",
	"local x : number | string = 1; local z = x + x",
	"local x : number | {oi: number} = 1; local y : number | {oi: number} = {oi= 1}; local f: boolean = x == y",
	"local x : string | number = 1; local y = x .. 'a'",
	"local function f(x: table | {} | {oi: number} | number): number return #x end",
	"local x : {oi : number} = {}",
	"local function f(x: {a: string} | {b: string}) local y: string = x.a end",
	"local function f(x: {a: string} | {b: string}) local y: nil = x.a end",
];

const lua51 = [
	...assignments,
	...comments,
	...conditionals,
	...do_,
	...escapesequences,
	...expressions,
	...for_,
	...functioncalls,
	...functions,
	...literals,
	...operators,
	...repeat,
	...return_,
	...statements,
	...tableconstructors,
	...while_,
	...extra,
];

const lua52 = [
	// escapesequences
	"a = '\\['",
	"a = '\\x0'",
	"a = '\\x0x'",
	"a = '\\xx'",
	// operators
	"a = 1 // 0",
	"a = p ~ q",
	"a = p & q",
	// statements
	"return;;",
	"::foo",
	'goto "foo"',
];

const lua53 = [
	// escapesequences
	"a = '\\u{110000}'",
	"a = '\\u{120000}'",
	"a = '\\u{100000001f4a9}'",
	"a = '\\u{1f4a9'",
	'a = "\\u{1f4a9"',
	"a = '\\uaa'",
	"a = '\\u{}'",
	"a = '\\u{0g}'",
	// operators
	"a = a <=",
	"a = a >=",
	...types,
];

const luajit = [
	// statements
	"goto goto",
];

describe("fails on necessary tests", () => {
	lua51.forEach(code =>
		it(code, () => expect(() => check(code, { luaVersion: "5.1" })).toThrow())
	);
	lua52.forEach(code =>
		it(code, () => expect(() => check(code, { luaVersion: "5.2" })).toThrow())
	);
	lua53.forEach(code =>
		it(code, () => expect(() => check(code, { luaVersion: "5.3" })).toThrow())
	);
	luajit.forEach(code =>
		it(code, () =>
			expect(() => check(code, { luaVersion: "LuaJIT" })).toThrow()
		)
	);
});
