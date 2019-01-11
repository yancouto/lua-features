#!/usr/bin/env node
// @flow
/* eslint no-console: "off" */

import { type LuaParseOptions, parseFile } from "./lua-parse";
import { check } from "./lua-type-check";
import { ConstVisitor } from "./const-visitor";
import fs from "fs";
import { generate } from "./lua-generator";
import { promisify } from "util";
import { visit } from "./visitor";
import yargs from "yargs";

// not assuming node 0.10
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

function getOptions(args: Object): LuaParseOptions {
	const features = {};
	if (args.const) features.const_ = true;
	if (args.typeCheck) features.typeCheck = true;
	return { features, luaVersion: args.withLua };
}

async function checkAll(args: Object) {
	try {
		process.chdir(args.srcDir);
		await forAllLuaFilesRecursive(
			".",
			"",
			async (dir: string, name: string) => {
				let ast = await parseFile(`./${dir}/${name}`, getOptions(args));
				if (args.typeCheck) ast = check(ast);
				if (args.const) visit(ast, [new ConstVisitor()]);
			}
		);
		console.log("No errors.");
	} catch (e) {
		console.error(e.toString());
		process.exit(1);
	}
}

async function transpileAll(args: Object) {
	try {
		let filesCompiled = 0;
		mkdir(args.outDir).catch(() => {});
		await forAllLuaFilesRecursive(
			args.srcDir,
			"",
			async (dir: string, name: string) => {
				const ast = await parseFile(
					`${args.srcDir}/${dir}/${name}`,
					getOptions(args)
				);
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
			describe: "Enables type-checking. (EXPERIMENTAL)",
		})
		.option("const", {
			alias: "c",
			type: "boolean",
			describe: "Enable const variables.",
		})
		.option("with-lua", {
			alias: "l",
			describe: "Lua version to be compatible with.",
			default: "5.1",
			choices: ["5.1", "5.2", "5.3", "JIT"],
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
		handler: checkAll,
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
		handler: transpileAll,
	})
	.demandCommand()
	.recommendCommands()
	.scriptName("lua-features")
	.help().argv;
