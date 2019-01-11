// @flow strict-local
import * as AST from "./ast-types";
import { astError, errors } from "./errors";
import { type Visitor } from "./visitor";

export class ConstVisitor implements Visitor {
	// true if the binding is const, false if local
	binds: Array<{ [name: string]: boolean }> = [];
	// Faster access
	var_binds: { [name: string]: Array<boolean> } = {};
	meta: AST.MetaInfo;

	addBind(name: string, const_: boolean): void {
		const b = this.binds;
		b[b.length - 1][name] = const_;
		const vb = this.var_binds;
		vb[name] = vb[name] || []; // creating array if missing
		vb[name].push(const_);
	}

	tryReassign(name: string, node: { ...AST.LocationInfo }): void {
		const vb = this.var_binds[name];
		if (vb != null && vb.length > 0 && vb[vb.length - 1] === true)
			throw astError(errors.cantReassignConst, this.meta, node);
	}

	LocalStatement = {
		enter: (node: AST.LocalStatement) => {
			node.variables.forEach(id =>
				this.addBind(id.name, node.kind === "const")
			);
		},
	};

	functionBase(node: { ...AST.FunctionBase }): void {
		// parameters for now are always considered non-const
		node.parameters.forEach(id => this.addBind(id.name, false));
	}

	LocalFunctionStatement = {
		enter: (node: AST.LocalFunctionStatement) => {
			this.addBind(node.identifier.name, node.kind === "const");
			this.functionBase(node);
		},
	};

	NonLocalFunctionStatement = {
		enter: (node: AST.NonLocalFunctionStatement) => {
			if (node.identifier.type === "Identifier")
				this.tryReassign(node.identifier.name, node.identifier);
			this.functionBase(node);
		},
	};

	AssignmentStatement = {
		enter: (node: AST.AssignmentStatement) => {
			node.variables.forEach(v => {
				if (v.type !== "Identifier") return;
				this.tryReassign(v.name, node);
			});
		},
	};

	Chunk = {
		enter: (node: AST.Chunk) => {
			this.meta = node.meta;
		},
	};

	createScope() {
		this.binds.push({});
	}

	destroyScope() {
		const vb = this.var_binds;
		for (const name in this.binds.pop()) {
			vb[name].pop();
			if (vb[name].length === 0) delete vb[name];
		}
	}
}
