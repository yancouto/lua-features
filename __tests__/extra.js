// @flow
/* eslint-env jest */
import { checkString as check } from "../src/lua-type-check";
import { tokenize } from "../src/lua-tokenize";

describe("extra tests", () => {
	it("works without storing comments", () =>
		expect(() => check("a = 1 -- comment", { comments: false })).not.toThrow());
	it("tokenize can process two instances in parallel", () => {
		const g1 = tokenize("a + 1", null, {});
		const g2 = tokenize("b - 2", null, {});
		expect((g1.next().value: any).value).toBe("a");
		expect((g2.next().value: any).value).toBe("b");
		expect((g1.next().value: any).value).toBe("+");
		expect((g2.next().value: any).value).toBe("-");
		expect((g1.next().value: any).value).toBe(1);
		expect((g2.next().value: any).value).toBe(2);
		expect(g2.next().done).toBe(true);
		expect(g1.next().done).toBe(true);
	});
});
