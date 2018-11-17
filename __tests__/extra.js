// @flow
/* eslint-env jest */
import { check } from "../src/lua-type-check";
import expect from "expect";

describe("extra tests", () => {
	it("works without storing comments", () =>
		expect(() => check("a = 1 -- comment", { comments: false })).not.toThrow());
	it("gets varargs correcty", () => {
		const body = check("function a(...) end").body;
		expect(body.length > 0);
		const node = body[0];
		expect(node.type === "FunctionDeclaration" && node.hasVarargs).toBeTruthy();
	});
});
