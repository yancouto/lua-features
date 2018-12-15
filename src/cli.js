#!/usr/bin/env node
// @flow
/* eslint no-console: "off" */

import { checkFile } from "./lua-type-check";
import { generate } from "./lua-generator";
import { parseFile } from "./lua-parse";
import yargs from "yargs";

async function check(args: Object) {
	try {
		await checkFile(args.file);
		console.log("No errors.");
	} catch (e) {
		console.error(e.toString());
		process.exit(1);
	}
}

async function transpileFile(args: Object) {
	try {
		const ast_p = args.typeCheck ? checkFile(args.file) : parseFile(args.file);
		const ast = await ast_p;
		console.log(generate(ast));
	} catch (e) {
		console.error(e.toString());
		process.exit(1);
	}
}

yargs
	.command({
		command: "check <file>",
		aliases: "$0",
		desc: "Type-check a file.",
		builder: yargs =>
			yargs.positional("file", {
				describe: "File to be type-checked",
				type: "string",
			}),
		handler: check,
	})
	.command({
		command: "transpile <file>",
		desc: "Transpile a file with type annotations to plain lua.",
		builder: yargs =>
			yargs
				.positional("file", {
					describe: "File to be transpiled",
					type: "string",
				})
				.option("c", {
					alias: "type-check",
					desc: "Also type-check the file",
					type: "boolean",
				}),
		handler: transpileFile,
	})
	.demandCommand()
	.recommendCommands()
	.scriptName("lua-type-check")
	.help().argv;
