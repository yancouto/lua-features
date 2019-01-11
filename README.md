# lua-features

lua-features is a static analyser for lua. It can parse code in Lua 5.1, 5.2, 5.3 and LuaJIT. Its code is adapted from [luaparse](https://github.com/oxyc/luaparse).

## Features

On top of interpreting usual Lua, it is also a transpiler that can add additional features on top of the language. All features are then transpiled to plain lua (see [CLI](#Command-line-interface)). Current features are:

### Const variables

Wherever you could use `local` (variable or function declaration) you can instead use `const`, this ensures that such variable cannot be reassigned.

```lua
const x = 1
x = 2 -- error: cannot reassign a constant
function x() end -- error: cannot reassign a constant
```

### Type checking

**This feature is experimental and does not work properly, avoid using it.** The plan is to add static type checking to lua, like [flow](https://flow.org) and [TypeScript](https://www.typescriptlang.org/) do to Javascript.

```lua
local x : number = 1;
x = "1" -- error: cannot assign string to number
function add(x, y: number, number): number
    return x + y
end
add(1, 'a') -- error: cannot call function
```

## Command line interface

lua-features provides a CLI (see [the code](src/cli.js)). Use the help command to get instructions.

```
lua-features <command>

Commands:
  lua-features check <src-dir>              Check all .lua files in src-dir.
  lua-features transpile <src-dir>          Transpile all .lua files in src-dir
  <out-dir>                                 to out-dir

Options:
  --version  Show version number                                       [boolean]
  --help     Show help                                                 [boolean]
```

The `check` command tries to compile all files in a directory:

```
lua-features check <src-dir>

Check all .lua files in src-dir.

Positionals:
  src-dir  Input directory                                   [string] [required]

Options:
  --version         Show version number                                [boolean]
  --help            Show help                                          [boolean]
  --type-check, -t  Enables type-checking.                             [boolean]
  --const, -c       Enable const variables.                            [boolean]
  --with-lua, -l    Lua version to be compatible with.
                          [choices: "5.1", "5.2", "5.3", "JIT"] [default: "5.1"]
```

This will print nicely formatted errors if your files don't compile.

```bash
# example.lua:
const x = 1;
x = 2;

# cli
lua-features check . --const

# output
[.//example.lua:2:1-6] cannot reassign a constant
    x = 2;
    ^^^^^
```

The `transpile` command transpiles all files from one directory to plain lua, and copies them to another directory.

```
lua-features transpile <src-dir> <out-dir>

Transpile all .lua files in src-dir to out-dir

Positionals:
  src-dir  Input Directory                                   [string] [required]
  out-dir  Output directory                                  [string] [required]

Options:
  --version         Show version number                                [boolean]
  --help            Show help                                          [boolean]
  --type-check, -t  Enables type-checking.                             [boolean]
  --const, -c       Enable const variables.                            [boolean]
  --with-lua, -l    Lua version to be compatible with.
                          [choices: "5.1", "5.2", "5.3", "JIT"] [default: "5.1"]
```

If you're using the const feature, for example, all consts are changed to local. Notice it transpiles even if `check` errors for some feature. The spacing is messed up for now, but may be fixed in the future. I tried to keep at least the same line numbers for all operations, that way if you get an error on line X in your transpiled code, it surely came from line X in the original code. The transpile `example.lua` file looks like:

```lua
local  a = 1
 a = 2
```

## Contributions

I accept PR's!

However, I will not lie: I'm not sure how much more time I'm gonna spend improving this repository :)

Possible feature ideas:
- Compatibility: for example, compile Lua 5.3 bitops to use LuaJIT bit library.
- continue: continue can probably be transpiled to some goto's