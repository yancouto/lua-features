const luatype = require("../lua-type-check");

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
  "a = a = 1"
];

const comments = ["--[=[x]x", "--[[\nbreak", "--[==[oi]]"];

const conditionals = [
  "if",
  "elseif",
  "else",
  "then",
  "if then",
  "if 1",
  "if 1 then",
  "if 1 else",
  "if 1 then else",
  "if 1 then elseif",
  "if 1 then elseif 2",
  "if 1 then elseif 2 then",
  "if 1 then else if 2 then end",
  "if 1 then return return end",
  "if then end",
  "if 1 then elseif then end"
];

const do_ = [
  "do",
  "end",
  "do 1 end",
  'do "foo" end',
  "do end do",
  "do end end",
  "do return return end"
];

const escapesequences = [
  'a = "bar\nbaz"',
  'a = "bar\rbaz"',
  "a = '\\255",
  "a = '\\256'",
  "a = '\\300'"
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
  "a = function(..., p) end"
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
  "for a = 1, 2 do return return end"
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
  "(a)\n()"
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
  "function a(...,p) end"
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
  "a = [=[x]x"
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
  "a = or 1"
];

const repeat = [
  "repeat",
  "repeat until",
  "repeat until local",
  "repeat end",
  "repeat 1",
  "repeat =",
  "repeat 2 until 1",
  'repeat "foo" until 1',
  "repeat return return until 0"
];

const return_ = ["return return", "return local", "return 1,"];

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
  "a = { 2 3 4 }"
];

const while_ = [
  "while",
  "while do",
  "while =",
  "while 1 do",
  "while 1 do 2 end",
  'while 1 do "foo" end',
  "while 1 do while",
  "while 1 end",
  "while 1 2 do",
  "while 1 = 2 do",
  "while 1 do return return end"
];

const extra = [
  "function a(p q) end",
  "function a(...) return function() print(...) end end",
  "... = 1"
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
  "local x = not 1",
  'local x = -"oi"',
  'local x = ~"oi"',
  "local x : number = y",
  "local x = 1 + '1'",
  "local x : number = f()",
  "while 1 do end",
  "local x : string = 'a'; while x do end",
  "while true do local x : number = '1' end",
  "local x : number = 1; while false do local x : string = 'a' end; local z = x .. 'b'",
  "local x : number = 1; repeat local x : string = 'a' until false; local z = x .. 'b'",
  "local x : number = 1; x = '1'",
  "local x : table = {a = true .. 'a'}",
  "local x : table = {['oi'] = true .. 'a'}",
  "local x : table = {true .. 'a'}"
];

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
  .concat(operators)
  .concat(repeat)
  .concat(return_)
  .concat(statements)
  .concat(tableconstructors)
  .concat(while_)
  .concat(extra)
  .concat(types);

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
  'goto "foo"'
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
  "a = a >="
];

const luajit = [
  // statements
  "goto goto"
];

describe("fails on necessary tests", () => {
  lua51.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "5.1" })).toThrow()
    )
  );
  lua52.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "5.2" })).toThrow()
    )
  );
  lua53.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "5.3" })).toThrow()
    )
  );
  luajit.forEach(code =>
    it(code, () =>
      expect(() => luatype.check(code, { luaVersion: "LuaJIT" })).toThrow()
    )
  );
});
