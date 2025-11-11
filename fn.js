#!/usr/bin/env node

const fs = require("fs")
const path = require("path")
const vm = require("node:vm")

// Cross-platform path to configuration directories.
const getConfigPath = () => process.env.XDG_CONFIG_HOME
	|| (process.env.HOME && path.join(process.env.HOME, ".config"))
	|| process.env.LOCALAPPDATA

let stack = []
let to_delete = new Set()

class DoNotReturn {}

// Remove element at `n` from `stack`, then return removed element.
const pop = (n) => {
	const index = stack.length - 1 - (n || 0)
	to_delete.add(index)
	return stack[index]
}

// Remove element at `n` of `stack`, returning nothing. Useful for shrinking the stack.
const del = (n) => {
	const index = stack.length - 1 - (n || 0)
	to_delete.add(index)
	return new DoNotReturn()
}

// Returns element at `n` of `stack`. Useful for growing the stack.
const get = (n) => {
	const index = stack.length - 1 - (n || 0)
	return stack[index]
}

const slurp = (n) => stack.splice(
	n === undefined ? 0 : stack.length - 1 - n,
	n ?? stack.length,
)

const P = pop
const G = get
const D = del

// Parses an array of arguments `args`. Valid arguments are hardcoded in this function.
const parseArgs = (args) => {
	const argKeys = {
		"i": "in",
		"o": "out",
		"r": "read",
		"w": "write",
		"n": "no-config",
		"h": "help",
	}
	const shortArgs = Object.keys(argKeys)
	const longArgs = Object.values(argKeys)

	// Default for each flag is false
	let out = {}
	for (const arg of longArgs) {
		out[`--${arg}`] = false
	}

	let freeArgs = []
	// After "--", all arguments should be treated as positional
	let freeArgsOnly = false

	// First two arguments are "node" and "fn.js"
	for (const arg of args.slice(2)) {
		if (freeArgsOnly) {
			freeArgs.push(arg)
		} else if (arg == "--") {
			freeArgsOnly = true
		} else if (arg.startsWith("--")) {
			if (longArgs.includes(arg.substr(2))) {
				out[arg.substr(2)] = true
			} else {
				throw new Error(`Invalid argument ${arg}`)
			}
		} else if (arg.startsWith("-")) {
			for (const c of arg.substr(1)) {
				if (shortArgs.includes(c)) {
					out[argKeys[c]] = true
				} else {
					throw new Error(`Invalid argument -${c}`)
				}
			}
		} else {
			freeArgs.push(arg)
		}
	}

	out["code"] = freeArgs
	return out
}

// Reads entire standard input to string.
// fs.readFileSync(0, "utf8") does not work because it may raise EAGAIN.
const readStdin = () =>
	new Promise(resolve => {
		let out = ""
		process.stdin.resume()
		process.stdin.setEncoding("utf8")
		process.stdin.on("data", (chunk) => {
			out += chunk
		})
		process.stdin.on('end', () => resolve(out))
	})

const HELP_TEXT = `fn-node [-h] [--in] [--out] [--read] [--write] [--no-config] [code ...]

positional arguments:
  code             JavaScript source code. Each returned value is pushed onto a stack.
                   The functions pop(), get(), and del() manipulate the stack.
                   They have short synonyms P(), G(), and D().

options:
  -h, --help       show this help message and exit
  --in, -i         Read STDIN as string.
  --out, -o        Print top of stack to STDOUT.
  --read, -r       Read stack from STDIN.
  --write, -w      Write stack to STDOUT.
  --no-config, -n  Disable configuration file.`

const main = async () => {
	const args = parseArgs(process.argv)
	if (args["help"]) {
		console.log(HELP_TEXT)
		return
	}

	const context = vm.createContext()
	context.P = P
	context.G = G
	context.D = D
	context.pop = pop
	context.get = get
	context.del = del
	context.slurp = slurp
	// Globals are set according to the documentation:
	// https://nodejs.org/docs/latest-v9.x/api/globals.html
	context.global = global
	for (const name in global) {
		context[name] = global[name]
	}
	// One could argue that these should be set to the directory of the process.
	// However, `process.cwd()` will yield the expected result just fine, so
	// setting `context.__dirname = process.cwd()` would be redundant.
	context.__dirname = __dirname
	context.__filename = __filename
	context.exports = exports
	context.module = module
	context.require = require
	context.Buffer = Buffer
	context.URL = URL
	context.process = process
	const myEval = (arg) => vm.createScript(arg).runInContext(context)

	if (!args["no-config"]) {
		const configPath = path.join(getConfigPath(), "fn", "config.js")
		if (fs.existsSync(configPath)) {
			myEval(fs.readFileSync(configPath, "utf-8"))
		}
	}

	if (args["in"]) {
		stack.push(await readStdin())
	} else if (args["read"]) {
		let str = await readStdin()
		// Parses multiple JSON objects in string
		while (true) {
			try {
				stack.push(JSON.parse(str))
				break
				// If string has more than one JSON object, raise error
			} catch (e) {
				const i = parseInt(e.message.match(/[0-9]+/))
				if (i == 0) {
					throw e
				} else {
					stack.push(JSON.parse(str.substr(0, i)))
					str = str.substr(i)
				}
			}
		}
	}

	// REPL
	for (const arg of args["code"]) {
		const value = await myEval(arg)
		if (!(value instanceof DoNotReturn)) {
			stack.push(value)
		}
		stack = stack.filter((_, i) => !to_delete.has(i))
		to_delete.clear()
	}

	if (args["out"]) {
		console.log(stack[stack.length - 1])
	} else if (args["write"]) {
		for (const obj of stack) {
			console.log(JSON.stringify(obj).trim())
		}
	}
}

main()
