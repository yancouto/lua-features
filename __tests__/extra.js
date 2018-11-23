// @flow
/* eslint-env jest */
import { check } from "../src/lua-type-check";
import expect from "expect";

describe("extra tests", () => {
	it("works without storing comments", () =>
		expect(() => check("a = 1 -- comment", { comments: false })).not.toThrow());
});
