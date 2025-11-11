#!/bin/env python3

import argparse
import ast
import sys
import os
import os.path


def get_config_path():
    """Cross-platform path to configuration directories."""
    # Linux support
    home = os.environ.get("HOME")
    xdg_config_home = os.environ.get("XDG_CONFIG_HOME")
    # Windows support
    localappdata = os.environ.get("LOCALAPPDATA")

    if xdg_config_home:
        return xdg_config_home
    if home:
        return os.path.join(home, ".config")
    if localappdata:
        return localappdata


stack = []
to_delete = {}


class DoNotReturn:
    pass


def pop(n=0):
    """Remove element at `n` from `stack`, then return removed element."""
    global to_delete
    index = len(stack) - (1 + n) if n >= 0 else abs(n) - 1
    to_delete[index] = True
    return stack[index]


def delete(n=0):
    """Remove element at `n` of `stack`, returning nothing. Useful for shrinking the stack."""
    global to_delete
    index = len(stack) - (1 + n) if n >= 0 else abs(n) - 1
    to_delete[index] = True
    return DoNotReturn()


def get(n=0):
    """Returns element at `n` of `stack`. Useful for growing the stack."""
    global to_delete
    index = len(stack) - (1 + n) if n >= 0 else abs(n) - 1
    return stack[index]


P = pop
G = get
D = delete


def main(args):
    global stack
    global to_delete
    # All execs and evals are kept in the same function to keep consistent context.

    # Configure
    if not args["no_config"]:
        config_path = os.path.join(get_config_path(), "fn", "config.py")
        if os.path.exists(config_path):
            with open(config_path, "rb") as fd:
                compiled = compile(fd.read(), "string", "exec")
                exec(compiled, globals())

    if args["in"]:
        stack.append(sys.stdin.read())
    elif args["read"]:
        module = ast.parse(sys.stdin.read())
        for child in ast.iter_child_nodes(module):
            compiled = compile(ast.Expression(child.value), "string", "eval")
            value = eval(compiled, globals())
            stack.append(value)

    # REPL
    for arg in args["code"]:
        module = ast.parse(arg)
        for child in ast.iter_child_nodes(module):
            if isinstance(child, ast.Expr):
                compiled = compile(ast.Expression(child.value), "string", "eval")
                value = eval(compiled, globals())
                if not isinstance(value, DoNotReturn):
                    stack.append(value)
            else:
                compiled = compile(ast.Module([child], []), "string", "exec")
                exec(compiled, globals())

        stack = [s for i, s in enumerate(stack) if not to_delete.get(i, False)]
        to_delete = {}

    if args["out"]:
        if stack:
            print(stack[-1])
    elif args["write"]:
        for e in stack:
            print(repr(e))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--in", "-i", action="store_true",
        help="Read STDIN as string."
    )
    parser.add_argument(
        "--out", "-o", action="store_true",
        help="Print top of stack to STDOUT."
    )
    parser.add_argument(
        "--read", "-r", action="store_true",
        help="Read stack from STDIN."
    )
    parser.add_argument(
        "--write", "-w", action="store_true",
        help="Write stack to STDOUT."
    )
    parser.add_argument(
        "--no-config", "-n", action="store_true",
        help="Disable configuration file."
    )
    parser.add_argument(
        "code", nargs="*",
        help="Python source code. Each returned value is pushed onto a stack. The functions pop(), get(), and delete() manipulate the stack. They have short synonyms P(), G(), and D()."
    )
    args = parser.parse_args()
    main(vars(args))
