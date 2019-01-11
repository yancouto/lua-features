#!/usr/bin/env node
// @flow
/* eslint no-console: "off" */

import { checkFile } from "./lua-type-check";
import fs from "fs";
import { generate } from "./lua-generator";
import { parseFile } from "./lua-parse";
import { promisify } from "util";
import yargs from "yargs";

const readdir = promisify(fs.readdir);
const mkdir = promisify(fs.mkdir);
const writeFile = promisify(fs.writeFile);
const stat = promisify(fs.stat);

async function forAllLuaFilesRecursive(
	base: string,
	path: string,
	callback: (string, string) => Promise<mixed>
): Promise<void> {
	const files: Array<string> = await readdir(`${base}/${path}`);
	const cbs = files
		.filter(f => f.endsWith(".lua"))
		.map(async f => {
			const stats: Object = await stat(`${base}/${path}/${f}`);
			if (stats.isFile()) await callback(path, f);
		});
	const recs = files.map(async f => {
		const stats: Object = await stat(`${base}/${path}/${f}`);
		if (stats.isDirectory())
			await forAllLuaFilesRecursive(base, `${path}/${f}`, callback);
	});
	await Promise.all([...cbs, ...recs]);
}

async function check(args: Object) {
	try {
		process.chdir(args.srcDir);
		await forAllLuaFilesRecursive(
			".",
			"",
			async (dir: string, name: string) => {
				await checkFile(`./${dir}/${name}`);
			}
		);
		console.log("No errors.");
	} catch (e) {
		console.error(e.toString());
		process.exit(1);
	}
}

async function transpile(args: Object) {
	try {
		let filesCompiled = 0;
		mkdir(args.outDir).catch(() => {});
		await forAllLuaFilesRecursive(
			args.srcDir,
			"",
			async (dir: string, name: string) => {
				const ast = await parseFile(`${args.srcDir}/${dir}/${name}`);
				await mkdir(`${args.outDir}/${dir}`).catch(() => {});
				await writeFile(`${args.outDir}/${dir}/${name}`, generate(ast));
				filesCompiled++;
			}
		);
		console.log(`Successfully transpiled ${filesCompiled} files.`);
	} catch (e) {
		console.error(e.toString());
		process.exit(1);
	}
}

function parseOptions(yargs: typeof yargs): typeof yargs {
	return yargs
		.option("type-check", {
			alias: "t",
			type: "boolean",
			describe: "Enables type-checking.",
		})
		.option("const", {
			alias: "c",
			type: "boolean",
			describe: "Enable const variables.",
		});
}

yargs
	.command({
		command: "check <src-dir>",
		desc: "Check all .lua files in src-dir.",
		builder: yargs =>
			parseOptions(
				yargs.positional("src-dir", {
					describe: "Input directory",
					type: "string",
				})
			),
		handler: check,
	})
	.command({
		command: "transpile <src-dir> <out-dir>",
		desc: "Transpile all .lua files in src-dir to out-dir",
		builder: yargs =>
			parseOptions(
				yargs
					.positional("src-dir", {
						describe: "Input Directory",
						type: "string",
					})
					.positional("out-dir", {
						describe: "Output directory",
						type: "string",
					})
			),
		handler: transpile,
	})
	.demandCommand()
	.recommendCommands()
	.scriptName("lua-features")
	.help().argv;
