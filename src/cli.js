#!/usr/bin/env node
// @flow
/* eslint no-console: "off" */

import { checkFile } from "./lua-type-check";
import { generate } from "./lua-generator";
import { parse } from "./lua-parse";
import yargs from "yargs";
import { promises as fs } from "fs";

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
		let filesCompiled = 0;
		// $FlowFixMe flow definition wrong
		fs.mkdir(args.outDir, { recursive: true });
		async function readDir(path: string) {
			const files: Object = await fs.readdir(`${args.srcDir}/${path}`, {
				withFileTypes: true,
			});
			const reads = files
				.filter(f => f.isFile() && f.name.endsWith(".lua"))
				.map(async f => {
					// $FlowFixMe flow definition wrong
					const str: string = await fs.readFile(
						`${args.srcDir}/${path}/${f.name}`,
						"utf8"
					);
					const ast = await parse(str);
					await fs.mkdir(`${args.outDir}/${path}`, { recursive: true });
					await fs.writeFile(`${args.outDir}/${path}/${f.name}`, generate(ast));
					filesCompiled++;
				});
			const writes = files
				.filter(f => f.isDirectory())
				.map(async f => readDir(`${path}/${f.name}`));
			return Promise.all([...reads, ...writes]);
		}
		await readDir("");
		console.log(`Successfully transpiled ${filesCompiled} files.`);
	} catch (e) {
		console.error(e.toString());
		process.exit(1);
	}
}

yargs
	.command({
		command: "check <file>",
		desc: "Type-check a file.",
		builder: yargs =>
			yargs.positional("file", {
				describe: "File to be type-checked",
				type: "string",
			}),
		handler: check,
	})
	.command({
		command: "transpile <src-dir> <out-dir>",
		aliases: "$0",
		desc: "Transpile all .lua files in src-dir to out-dir",
		builder: yargs =>
			yargs
				.positional("src-dir", {
					describe: "Input Directory",
					type: "string",
				})
				.positional("out-dir", {
					describe: "Output directory",
					type: "string",
				}),
		handler: transpileFile,
	})
	.demandCommand()
	.recommendCommands()
	.scriptName("lua-type-check")
	.help().argv;
