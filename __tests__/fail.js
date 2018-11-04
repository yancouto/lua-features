const luaparse = require("../luaparse");

const fail = ["local a = 12 : number"];

it("fails on necessary tests", () => {
  fail.forEach(code => expect(() => luaparse.parse(code)).toThrow());
});
