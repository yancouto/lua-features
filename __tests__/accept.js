// @flow strict-local
/* eslint-env jest */
import { checkString as check } from "../src/lua-type-check";

const assignments = [
	"a = 1",
	"a = 1,2,3",
	"a,b,c = 1",
	"a,b,c = 1,2,3",
	"a.b = 1",
	"a.b.c = 1",
	"a[b] = 1",
	"a[b][c] = 1",
	"a.b[c] = 1",
	"a[b].c = 1",
	"a[b], a[c] = 1",
	"a().b = 1",
	"({})[b] = 1",
	'a""[b] = 1',
	"a{}[b] = 1",
	"({{}})[a][b] = 1",
	"(a).b = 1",
	"(a(1)).a = 0",
];

const comments = [
	"-- comment",
	"if true-- comment\nthen end",
	"break--comment",
	"--[[comment]]",
	"if true--[[comment]]then end",
	"--[=[comment]=]break",
	"if true--[===[comment\n--[=[sub]=]--\n]===]then end",
	"--[[comment\nline two]]",
	"--[[\ncomment\nline two\n]]",
	"if true--[==\nthen end --]]",
];

const conditionals = [
	"if true then end",
	"if true then local a end",
	"if true then local a local b end",
	"if true then local a; local b; end",
	"if true then else end",
	"if true then local a else local b end",
	"if true then local a; else local b; end",
	"if true then elseif x == 2 then end",
	"if true then local a elseif x == 2 then local b end",
	"if true then local a; elseif x == 2 then local b; end",
	"if true then elseif x == 2 then else end",
	"if true then else if x == 2 then end end",
	"if true then return end",
	"if true then end; if true then end;",
];

const do_ = [
	"do end",
	"do local a, b end",
	"do local a local b end",
	"do local a; local b; end",
	"do local a = 1 end",
	"do do end end",
	"do do end; end",
	"do do do end end end",
	"do do do end; end; end",
	"do do do return end end end",
	"do return end",
];

const escapesequences = [
	'a = "bar\\n\\r\\t\tbaz"',
	'a = "bar\\80baz\\a\\80\\0foo"',
	'a = "bar\\f\\v\\bbaz"',
	'a = "bar\f\v\baz"',
	"a = '\\\\'",
	"a = '\\''",
	"a = '\\['",
	"a = '\\123'",
	"a = '\\255'",
	"a = [[bar\\f\\v\\bbaz]]",
	"a = [[\\]]",
	"a = '\\z \\x'",
	"a = '\\x23'",
];

const expressions = [
	"a = [[foo]]",
	"a = {}",
	"a = (a)",
	"a = (nil)",
	"a = (true)",
	"a = (1)",
	'a = ("foo")',
	"a = ([[foo]])",
	"a = ({})",
	"a = a.b",
	"a = a.b.c",
	"a = a[b]",
	"a = a[1]",
	'a = a["foo"]',
	"a = function() end",
	"a = function(p) end",
	"a = function(p,q,r) end",
	"a = function(...) end",
	"a = function(p,...) end",
	"a = function(p,q,r,...) end",
	"a = {'-'}",
	"a = {'not'}",
	"a = {not true}",
];

const for_ = [
	"for a in b do end",
	"for a in b do local a local b end",
	"for a in b do local a; local b; end",
	"for a, b, c in p do end",
	"for a, b, c in p, q, r do end",
	"for a in 1 do end",
	"for a in true do end",
	'for a in "foo" do end',
	"for a in b do break end",
	"for a in b do return end",
	"for a in b do do end end",
	"for a in b do do break end end",
	"for a in b do do return end end",
	"for a = p, q do end",
	"for a = 1, 2 do end",
	"for a = 1, 2 do local a local b end",
	"for a = 1, 2 do local a; local b; end",
	"for a = p, q, r do end",
	"for a = 1, 2, 3 do end",
	"for a = p, q do break end",
	"for a = 1, 2 do return end",
	"for a = p, q do do end end",
	"for a = p, q do do break end end",
	"for a = p, q do do return end end",
];

const functioncalls = [
	"a()",
	"a(1)",
	"a(1,2,3)",
	"a()()",
	"a.b()",
	"a[b]()",
	"a.b.c()",
	"a[b][c]()",
	"a[b].c()",
	"a.b[c]()",
	"a:b()",
	"a.b:c()",
	"a[b]:c()",
	"a:b():c()",
	"(a)()",
	'(a("foo"))()',
	"(a)()()",
	"(a).b()",
	"(a)[b]()",
	"(a):b()",
	'a"foo"',
	"a[[foo]]",
	'a.b"foo"',
	'a[b]"foo"',
	'a:b"foo"',
	'a()"foo"',
	'a"foo"()',
	'a"foo".b()',
	'a"foo"[b]()',
	'a"foo":c()',
	'a"foo""bar"',
	'a"foo"{}',
	'(a):b"foo".c[d]:e"bar"',
	'(a):b"foo"[c].d:e"bar"',
	'a{}"foo"',
	"a{}",
	"a.b{}",
	"a[b]{}",
	"a:b{}",
	"a(){}",
	"a{}()",
	"a{}.b()",
	"a{}[b]()",
	"a{}:c()",
	"a{}{}",
	"(a):b{}.c[d]:e{}",
	"(a):b{}[c].d:e{}",
	'a\n""',
	"a\n{}",
	'a.b\n""',
	"a.b\n{}",
	'a:b\n""',
	"a:b\n{}",
	'(a)\n""',
	"(a)\n{}",
];

const functions = [
	"function a() end",
	"function a(p) end",
	"function a(p,q,r) end",
	"function a(p) return end",
	"function a(p) do end end",
	"function a.b() end",
	"function a.b.c.d() end",
	"function a:b() end",
	"function a.b.c:d() end",
	"function a(...) end",
	"function a(p,...) end",
	"function a(p,q,r,...) end",
	"function a() local a local b end",
	"function a() local a; local b; end",
	"function a() end; function a() end;",
];

const literals = [
	"a = 1",
	"a = .1",
	"a = 1.1",
	"a = 10.1",
	"a = 1e1",
	"a = 1E1",
	"a = 1e+9",
	"a = 1e-1",
	"a = 0xf",
	"a = 0xf.",
	"a = 0xf.3",
	"a = 0xfp1",
	"a = 0xfp+1",
	"a = 0xfp-1",
	"a = 0xFP+9",
	"a = '1' .. '3' .. '-2'",
	"a = '1' .. \"bar\"",
	"a = 'bar'",
	'a = "bar"',
	"a = nil",
	"a = true",
	"a = false",
	"a = ...",
	"a = [[hello\nworld]]",
	"a = { 1, 2, [[hello\n\nworld]], \n 3 }",
	'a = "\\\r"',
	'a = "\\\n"',
	'a = "\\\r\n"',
	'a = "\\\n\r"',
];

const local = [
	"local a",
	"local a;",
	"local a, b, c",
	"local a; local b local c;",
	"local a = 1",
	"local a local b = a",
	"local a, b = 1, 2",
	"local a, b, c = 1, 2, 3",
	"local a, b, c = 1",
	"local a = 1, 2, 3",
	"local function a() end",
	"local function a(p) end",
	"local function a(p,q,r) end",
	"local function a(p) return end",
	"local function a(p) do end end",
	"local function a(...) end",
	"local function a(p,...) end",
	"local function a(p,q,r,...) end",
	"local function a() local a local b end",
	"local function a() local a; local b; end",
	"local function a() end; local function a() end;",
	"local a = { b = { z = 1 } }",
];

const operators = [
	"a = -f()",
	"a = not true",
	"a = 1 + -2; a = 1 - -2",
	"a = 1 * (-2); a = 1 / -2",
	"a = 1 + 2 - 3 * 4 / 5 ^ 6",
	"a = a + b - c",
	'a = "foo" .. "bar"',
	'a = "foo".."bar".."baz"',
	"a = f() + g() - h()",
	"a = f() * g()",
	"a = f() / g()",
	"a = a() ^ b()",
	"a = 1 == 2; a = 1 ~= 2",
	"a = 1 < 2; a = 2 <= 2; a = 2 > 2; a = 2 >= 2; a = 2 == 2",
	"a = a ~= b",
	"a = 1 and 2 or 2",
	"a = {} and {} or {}",
	'a = (1) and ("foo") or (nil)',
];

const repeat = [
	"repeat until false",
	"repeat until false",
	"repeat local a until true",
	"repeat local a local b until false",
	"repeat local a; local b; until false",
	"repeat return until false",
	"repeat break until false",
	"repeat do end until false",
	"repeat do return end until false",
	"repeat do break end until false",
];

const return_ = [
	"function f(): number return 1 end",
	'function f() return "foo" end',
	"function f(): number, number, number return 1,2,3 end",
	"function f() return a,b,c,d end",
	"function f() : number, number return 1,2; end",
];

const scope = [
	"local foo = 1 do foo = 2 end",
	"do local foo = 1 end foo = 2",
	"do local foo = 1 end do foo = 2 end",
	"local foo do foo = 1 do foo = 2 end end",
	"local function foo() end foo()",
	"local a = { a }",
	"local b = { b, b.a, b[a], b:a() }",
	"local b: table = {} local a = { b, b.a, b[a], b:a() }",
	"local c local a = { b[c] }",
	"local a = function() end a()",
	"local a, b = 1, a",
	"local a, b = 1, function() b = 2 end",
	"local a (a):b():c()",
	"local a, b for i, a, b in c do end",
	"local a, b, c for i, a, b in c do end",
	"local a: table = {} function a:b(): table return self end self = nil",
	"repeat local a = true until a",
	"local a = function (b) end b = 0",
	"for a = 1, 5 do end a = 0",
];

const statements = ["break", "return;", 'goto "foo"'];

const tableconstructors = [
	"a = {}",
	"a = {{{}}}",
	"a = {{},{},{{}},}",
	"a = { 1 }",
	"a = { 1, }",
	"a = { 1; }",
	"a = { 1, 2 }",
	"a = { a, b, c, }",
	"a = { true; false, nil; }",
	"a = { a.b, a[b]; a:c(), }",
	'a = { 1 + 2, a > b, "a" .. "b" }',
	"a = { a=1, }",
	'a = { a=1, b="foo", c=nil }',
	'a = { 1, a="foo"; b={}, d=true; }',
	'a = { ["foo"]="bar" }',
	"a = { [1]=a, [2]=b, }",
	'a = { true, a=1; ["foo"]="bar", }',
];

const while_ = [
	"while true do end",
	"while true do local a end",
	"while true do local a local b end",
	"while true do local a; local b; end",
	"while true do end",
	"while true do return end",
	"while true do do end end",
	"while true do do return end end",
	"while true do break end",
	"while true do do break end end",
];

const types = [
	"",
	"local x : number = 1; local y : any; local z : number = x + y",
	"local a : number = 2",
	"local a, b : number, boolean = 1, true",
	"a = function(p, q: string, nil) end",
	'a = function(p : string) : string return p .. "hi" end',
	"local x = function (a, b : number, boolean): nil if b then print(a) end end",
	"local a : number = 1; local b : number = a",
	"local a : number = 1 + 2 * 3 / 4 % 5",
	"local x : boolean = 0 < 1",
	'local x : boolean = "oi" >= "tchau"',
	`local x : boolean = 'o' <= "tchau"`,
	'local x : boolean = (1 < 2) == ("a" < "b")',
	"local x : any = (1 ~= 2) or false",
	'local s : boolean = ("a" .. "b") > (("cd"))',
	"local x : number = -1 + ~1",
	"local x : number = #{1}",
	"local x : boolean = not true",
	"local x : boolean = y == z",
	"f()",
	"local x : any = f()",
	"while true do local a: number = 1; local a: string = '1' end",
	"local x : number = 3; while x == 2 do end",
	"local x : number = 1; repeat	local x : string = 'a' until x == 'a'; local z: number = x + 1",
	"local x : number = 1; x = 3",
	"local x : table = {false, a = {}, ['b' .. '3'] = z}",
	"({a=2}).a = 1",
	"local x : any = 1; x.a = 2",
	"(function() end)()",
	"local x : number = 1; (function(x : string) end)('a'); x = 3",
	"local f : number = 1; local function f() end; f()",
	"local f : function = function() end",
	"local a : table = {}; a.b()",
	"if a == b then end",
	"local a : nil = nil",
	"local x : number = 1; for i = x, x + 2 do end",
	"local a : number = 1; do local a : string = 'oi'; local b : string = a end",
	"function f(... : number, string) local a, x : number, string = ... end",
	"function f(... : number, string, string) local a, x : number, number = ..., 1 end",
	"local f : () => () = function() end",
	"(function(... : string, number) end)('a', 1)",
	"function f(a, b): any return a end",
	"(function(a, b) end)()",
	"function f() return end",
	"local function f(): void return end",
	// TODO this will only work properly when I get tables and "global vars" working
	//"function f(a, b : number, number) end \n function g(): number, number return 1, 2 end \n f(g())",
	"function f(a, b : number, number) end \n f(1, 2)",
	"local function f(a, b : number, number) end \n f(1, 2)",
	"local function f(a, b : number, number) end \n local function g(): number, number return 1, 2 end \n f(g())",
	"local function f(): number, string return 1, 'a' end\n local a, b : number, string = f()",
	"local f : (number) => (string) = function(a: number): string return 'a' end\nlocal x : string = f(1)",
	"local f : (() => ()) => (() => ()) = function(a: () => ()): () => () return a end\n f(function() print('test') end)",
	"local f : (number) => (string) = function(a, b, c): string return 'a' end",
	"local f : (number) => (string) = function(...): string return 'a' end",
	"local f : () => (string) = function(): string return 'a' end",
	"local function f(): number return 1 end\n local a : number = f()",
	"local function f(): void return end",
	"local function f(): void end\nlocal function g(): void return f() end",
	"local function f(...: number, string) local a, b, c, d, e : number, boolean, number, string, nil = ..., true, ... end",
	"local function f(): number, string return 1, 'a' end\nlocal a, b, c, d : number, number, number, string = f(), f(), f()",
	"local function f(... :number, number) local a, b : number, nil = (...) end",
	"local x : {oi: number} = {oi = 1}\nx.oi = 10",
	"local function f(): {a: number} return {a = 1} end\nlocal x: number = f().a",
	"local function f(): {a: {a: number}} return {a = {a = 1}} end\nlocal x = 1 + f().a.a",
	"local x : {cb: () => ()} = {cb = function() end}\nx.cb()",
	"local y : {a: number, b: string} = {a = 1, b = 'a'}\nlocal function f(a, b : string, number): void end\nf(y.b, y.a)",
	"local y : {a: number, b: string,} = {a = 1, b = 'a'}\nlocal function f(a, b : string, number): void end\nf(y.b, y.a)",
	"local x : number | string = 1; x = 'a'",
	"local x : number | string = 1; local y : number | string | nil = x;",
	'local function f(a: number | string): void end; f(1); f "oi"; f("oi"); local x : number | string = 1; f(x)',
	"local function f(): number | string; return 1; end; local x : number | string | nil = f()",
	"local function f(): number | string | nil end",
	"local function f(a: () => (string) | () => (number)) local x : number | string = a() end",
	"local function f(a: (number) => (number) | (string) => (string)); local x: any; a(x) end",
	"local function f(x, y: number | nil, string | nil) local a : number | string | nil = x or y end",
	// TODO This should work in the future, since we know because of the short circuit or that
	// either x is truthy or we pick y.
	//"local function f(x, y: number | nil, string) local a : number | string = x or y end",
	"local function f(x: table | {} | {oi: number}): number return #x end",
	"local function f(a: table | {}) end; f{} f({})",
	"local f: (number | string) => (boolean | nil) = function(a: number | string): boolean | nil; return true; end",
	"local x: {a: number | string} = {a = 1}; x = {a = 'a'}; local y : number | string = x.a",
	"local x: {a: number} | {a: string} = {a = 1}; x = {a = 'a'}; local y : number | string = x.a",
	"local function f(x: {a: number} | {a: string}) local y : {a: number | string} = x; end",
	// TODO Should this really work? If so, how?
	//"local function f(x: {a: number | string}) local y : {a: number} | {a: string} = x; end",
	"local function f(x: {c1: {c2: number | string}}) local y : number | string = x.c1.c2 end",
	"local function f(x: {c1: {c2: number}| {c2: string}}) local y : number | string = x.c1.c2 end",
	"local function f(x: {a: string} | {b: string}) local y : string | nil = x.a; local z : string | nil = x.b; end",
	"local function f(x: {}) local y : nil = x.test end",
	"local x = 1; x = 2",
	"local x: number | string = 1; x = 'aaa'",
	// declare
	"declare x: number; local function f(): number; return x; end; local y: number = x;",
	"declare f: (number) => (string); local function f(x: number): number; return x + 1; end; local y: number = f(2)",
	"declare f: (number) => (string); local x: string = f(1)",
	//
	": number return 1",
];

const extra = [
	"#!/bin/env lua\na = 2",
	"local function f(a, ...) if true then print(...) end end",
	"a['oi']:get().x = 1",
	"a = ({1})",
	"a(1, 2, ...)",
	"function a.b.c.d.e.f() end",
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
	...local,
	...operators,
	...repeat,
	...return_,
	...scope,
	...statements,
	...tableconstructors,
	...while_,
	...extra,
];

const lua52 = [
	// escapesequences
	'a = "bar\\z    baz"',
	// functioncalls
	"a\n()",
	"a.b\n()",
	"a:b\n()",
	"(a)\n()",
	// statements
	"return;",
	";",
	"::foo::",
	"goto foo",
	";;;;;;",
];

const lua53 = [
	// escapesequences
	"a = '\\u{1f4a9}'",
	"a = '\\u{7f}\\u{80}'",
	"a = '\\u{7ff}\\u{800}'",
	"a = '\\u{ffff}\\u{1f4a9}'",
	"a = '\\u{10ffff}'",
	"a = '\\u{000000001f4a9}'",
	// operators
	"a = 1 // 0",
	"a = p ~ q >> r | s",
	"a = ~ p ~ q / r",
	// extra
	"a = (1 << 12)",
	// types
	"local x : boolean = (2 << 3) > (4 >> 5)",
	"local x : number = (1 << 2) & (3 | 4) & ~5",
	...types,
];

const luajit = [
	// statements
	"::foo::",
	'goto "foo"',
	"goto foo",
];

const extendedIdentifiers = [
	// local
	"local Дождь = {}",
];

describe("fails on necessary tests", () => {
	lua51.forEach(code =>
		it(code, () =>
			expect(() => check(code, { luaVersion: "5.1" })).not.toThrow()
		)
	);
	lua52.forEach(code =>
		it(code, () =>
			expect(() => check(code, { luaVersion: "5.2" })).not.toThrow()
		)
	);
	lua53.forEach(code =>
		it(code, () =>
			expect(() => check(code, { luaVersion: "5.3" })).not.toThrow()
		)
	);
	luajit.forEach(code =>
		it(code, () =>
			expect(() => check(code, { luaVersion: "LuaJIT" })).not.toThrow()
		)
	);
	extendedIdentifiers.forEach(code =>
		it(code, () =>
			expect(() => check(code, { extendedIdentifiers: true })).not.toThrow()
		)
	);
});
