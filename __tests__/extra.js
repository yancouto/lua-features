// @flow strict-local
/* eslint-env jest */
import { check } from "../src/lua-type-check";

describe("extra tests", () => {
	it("works without storing comments", () =>
		expect(() => check("a = 1 -- comment", { comments: false })).not.toThrow());
});
