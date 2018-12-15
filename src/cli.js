#!/usr/bin/env node
// @flow
/* eslint no-console: "off" */

import { check } from "./lua-type-check";
import fs from "fs";
import { generate } from "./lua-generator";
import { parse } from "./lua-parse";
import yargs from "yargs";

function checkFile(args: Object) {
	fs.readFile(args.file, (err, data) => {
		if (err) throw new Error(`Can't read file "${args.file}"`);
		try {
			check(data.toString());
			console.log("No errors.");
		} catch (e) {
			console.error(e.toString());
			process.exit(1);
		}
	});
}

function transpileFile(args: Object) {
	fs.readFile(args.file, (err, data) => {
		if (err) throw new Error(`Can't read file "${args.file}"`);
		try {
			const ast = args.typeCheck
				? check(data.toString())
				: parse(data.toString());
			console.log(generate(ast));
		} catch (e) {
			console.error(e.toString());
			process.exit(1);
		}
	});
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
		handler: checkFile,
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
