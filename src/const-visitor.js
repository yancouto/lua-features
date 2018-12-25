// @flow strict
import type { Visitor } from "./visitor";
import * as AST from "./ast-types";

export class ConstVisitor implements Visitor {
	// true if the binding is const
	binds: Array<{ [name: string]: boolean }>;
	LocalStatement: $NonMaybeType<$PropertyType<Visitor, "LocalStatement">>;
	LocalFunctionStatement: $NonMaybeType<
		$PropertyType<Visitor, "LocalFunctionStatement">
	>;
	AssignmentStatement: $NonMaybeType<
		$PropertyType<Visitor, "AssignmentStatement">
	>;

	constructor() {
		this.binds = [];

		this.LocalStatement = {
			enter: (node: AST.LocalStatement) => {
				const b = this.binds;
				node.variables.forEach(
					id => (b[b.length - 1][id.name] = node.kind === "const")
				);
			},
		};

		this.LocalFunctionStatement = {
			enter: (node: AST.LocalFunctionStatement) => {
				const b = this.binds;
				b[b.length - 1][node.identifier.name] = node.kind === "const";
			},
		};

		this.AssignmentStatement = {
			enter: (node: AST.AssignmentStatement) => {
				const b = this.binds;
				node.variables.forEach(v => {
					if (v.type !== "Identifier") return;
					for (let i = b.length - 1; i >= 0; i--)
						if (b[i][v.name] != null) {
							if (b[i][v.name] === true)
								throw new Error("Can't reassign constant.");
							// If it is false, it is fine to reassign a local variable
							return;
						}
				});
			},
		};
	}

	createScope() {
		this.binds.push({});
	}

	destroyScope() {
		this.binds.pop();
	}
}
