const luatype = require("../lua-type-check");

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
  "(1).a = 0"
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
  "if true--[==\nthen end --]]"
];

const conditionals = [
  "if 1 then end",
  "if 1 then local a end",
  "if 1 then local a local b end",
  "if 1 then local a; local b; end",
  "if 1 then else end",
  "if 1 then local a else local b end",
  "if 1 then local a; else local b; end",
  "if 1 then elseif 2 then end",
  "if 1 then local a elseif 2 then local b end",
  "if 1 then local a; elseif 2 then local b; end",
  "if 1 then elseif 2 then else end",
  "if 1 then else if 2 then end end",
  "if 1 then return end",
  "if 1 then end; if 1 then end;"
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
  "do return end"
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
  "a = '\\x23'"
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
  "a = {not true}"
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
  "for a = p, q do do return end end"
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
  '("foo")()',
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
  '(a\n"")',
  "(a\n{})",
  '(a)\n""',
  "(a)\n{}"
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
  "function a() end; function a() end;"
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
  "a = 1 .. 3 .. -2",
  'a = 1 .. "bar"',
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
  'a = "\\\n\r"'
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
  "local a = { b = { z = 1 } }"
];

const operators = [
  'a = -"foo"',
  "a = not 10",
  "a = 1 + -2; a = 1 - -2",
  "a = 1 * not 2; a = 1 / not 2",
  "a = 1 + 2 - 3 * 4 / 5 ^ 6",
  "a = a + b - c",
  'a = "foo" + "bar"',
  'a = "foo".."bar".."baz"',
  "a = true + false - nil",
  "a = {} * {}",
  "a = function() end / function() end",
  "a = a() ^ b()",
  "a = 1 == 2; a = 1 ~= 2",
  "a = 1 < 2 <= 2 > 2 >= 2 == 2",
  "a = a ~= b",
  "a = 1 and 2 or 2",
  "a = {} and {} or {}",
  'a = (1) and ("foo") or (nil)'
];

const repeat = [
  "repeat until 0",
  "repeat until false",
  "repeat local a until 1",
  "repeat local a local b until 0",
  "repeat local a; local b; until 0",
  "repeat return until 0",
  "repeat break until 0",
  "repeat do end until 0",
  "repeat do return end until 0",
  "repeat do break end until 0"
];

const return_ = [
  "return 1",
  'return "foo"',
  "return 1,2,3",
  "return a,b,c,d",
  "return 1,2;"
];

const scope = [
  "local foo = 1 do foo = 2 end",
  "do local foo = 1 end foo = 2",
  "do local foo = 1 end do foo = 2 end",
  "local foo do foo = 1 do foo = 2 end end",
  "local function foo() end foo()",
  "local a = { a }",
  "local b = { b, b.a, b[a], b:a() }",
  "local b = {} local a = { b, b.a, b[a], b:a() }",
  "local c local a = { b[c] }",
  "local a = function() end a()",
  "local a, b = 1, a",
  "local a, b = 1, function() b = 2 end",
  "local a (a):b():c()",
  "local a, b for i, a, b in c do end",
  "local a, b, c for i, a, b in c do end",
  "local a = {} function a:b() return self end self = nil",
  "repeat local a = true until a",
  "local a = function (b) end b = 0",
  "for a = 1, 5 do end a = 0"
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
  'a = { 1 + 2, a > b, "a" or "b" }',
  "a = { a=1, }",
  'a = { a=1, b="foo", c=nil }',
  'a = { 1, a="foo"; b={}, d=true; }',
  'a = { ["foo"]="bar" }',
  "a = { [1]=a, [2]=b, }",
  'a = { true, a=1; ["foo"]="bar", }'
];

const while_ = [
  "while 1 do end",
  "while 1 do local a end",
  "while 1 do local a local b end",
  "while 1 do local a; local b; end",
  "while true do end",
  "while 1 do return end",
  "while 1 do do end end",
  "while 1 do do return end end",
  "while 1 do break end",
  "while 1 do do break end end"
];

const types = [
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
  "local x : boolean = (1 ~= 2) or false",
  'local s : boolean = ("a" .. "b") > (("cd"))',
  "local x : number = -1 + ~1",
  "local x : number = #{1}",
  "local x : boolean = not true",
  "local x : number = y == z",
  "f()",
  "local x : any = f()",
  "while true do local a: number = 1; local a: string = '1' end",
  "local x : number = 3; while x == 2 do end",
  "local x : number = 1; repeat	local x : string = 'a' until x == 'a'; local z: number = x + 1",
  // "local a : number = 1; do local a : string = 'oi'; local b : string = a end"
  ""
];

const extra = ["#!/bin/env lua\na = 2", "local function f(a, ...) end"];

const lua51 = []
  .concat(assignments)
  .concat(comments)
  .concat(conditionals)
  .concat(do_)
  .concat(escapesequences)
  .concat(expressions)
  .concat(for_)
  .concat(functioncalls)
  .concat(functions)
  .concat(literals)
  .concat(local)
  .concat(operators)
  .concat(repeat)
  .concat(return_)
  .concat(scope)
  .concat(statements)
  .concat(tableconstructors)
  .concat(while_)
  .concat(types)
  .concat(extra);

const lua52 = [
  // escapesequences
  'a = "bar\\z    baz"',
  // functioncalls
  "a\n()",
  "a.b\n()",
  "a:b\n()",
  "(a\n())",
  "(a)\n()",
  // statements
  "return;",
  ";",
  "::foo::",
  "goto foo"
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
  "local x : number = (1 << 2) & (3 | 4) & ~5"
];

const luajit = [
  // statements
  "::foo::",
  'goto "foo"',
  "goto foo"
];

const extendedIdentifiers = [
  // local
  "local Дождь = {}"
];

describe("fails on necessary tests", () => {
  lua51.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "5.1" })).not.toThrow()
    )
  );
  lua52.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "5.2" })).not.toThrow()
    )
  );
  lua53.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "5.3" })).not.toThrow()
    )
  );
  luajit.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "LuaJIT" })).not.toThrow()
    )
  );
  extendedIdentifiers.forEach(code =>
    it(code, () =>
      expect(() =>
        luatype.check(code, { extendedIdentifiers: true })
      ).not.toThrow()
    )
  );

  // some extra tests
  it("works without storing comments", () =>
    expect(() =>
      luatype.check("a = 1 -- comment", { comments: false })
    ).not.toThrow());
});
