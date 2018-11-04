const luaparse = require("../luaparse");

const accept = [
  "local x = function (a, b : number, boolean): nil if b then print(a) end end",
  "local function a() local a local b end"
];

it("fails on necessary tests", () => {
  accept.forEach(code => expect(() => luaparse.parse(code)).not.toThrow());
});
