# fn

`fn` is a program that executes code provided as arguments. It also has utility arguments for reading and writing to standard input and output as strings or readable data.

`fn` is designed to be used ad hoc in a shell.

`fn` supports the following languages.

- Common Lisp: `fn-lisp`
- Python 3: `fn-py`
- Node.js: `fn-node`

## Usage

In general, each language has utilities to read from standard input, write to standard output, and manage its configuration file.

```
fn-lisp OPTIONS... CODE...

OPTIONS:
  -h, --help            Print this help text.
  -i, --in              Read STDIN as string.
  -o, --out             Print top of stack to STDOUT.
  -r, --read            Read stack from STDIN.
  -w, --write           Write stack to STDOUT.
  -m, --no-reader-macro Disable reader macros (. -> s-delete, % -> s-pop, $ -> s-get).
  -n, --no-config       Disable configuration file.

CODE: Common Lisp source code. Each returned value is pushed onto a stack. The following macros are available:
   %  Pops top element of stack (fn:s-pop).
   %n Pops element n from stack (fn:s-pop n).
   $  Gets top element of stack (fn:s-get).
   .  Deletes top element of stack (fn:s-delete).
```

```
fn.py [-h] [--in] [--out] [--read] [--write] [--no-config] [code ...]

positional arguments:
  code             Python source code. Each returned value is pushed onto a stack.
                   The functions pop(), get(), and delete() manipulate the stack.
                   They have short synonyms P(), G(), and D().

options:
  -h, --help       show this help message and exit
  --in, -i         Read STDIN as string.
  --out, -o        Print top of stack to STDOUT.
  --read, -r       Read stack from STDIN.
  --write, -w      Write stack to STDOUT.
  --no-config, -n  Disable configuration file.
```

```
fn-node [-h] [--in] [--out] [--read] [--write] [--no-config] [code ...]

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
  --no-config, -n  Disable configuration file.
```

## The Stack

Every top-level expression is evaluated and pushed onto a stack. Combined with the stack manipulation macros `%`, `\$`, and `.`, data can be "piped" between expressions. This enables code to be written more linearly, which is important since text editing and balancing nested parentheses is difficult on most terminals.

```sh
# Pushing values to stack
$ fn-lisp -w 1 3
$ fn-py -w 1 3
$ fn-node -w 1 3
1
3

# Popping values from stack
$ fn-lisp -w '1 3 (+ % %)'
$ fn-py -w 1 3 'P() + P()'
$ fn-node -w 1 3 'P() + P()'
4

# Copying top of stack
$ fn-lisp -w '1 3 (+ $ $)'
$ fn-py -w 1 3 'G() + G()'
$ fn-node -w 1 3 'G() + G()'
1
3
6

# Copying nth from top of stack
$ fn-lisp -w '1 3 (+ $0 $1)'
$ fn-py -w 1 3 'G(0) + G(1)'
$ fn-node -w 1 3 'G(0) + G(1)'
1
3
4
```

## Configuration

The following file will be loaded before arguments are executed:

- Common Lisp
  - Unix: `$XDG_CONFIG_HOME/fn/config.lisp` or `$HOME/.config/fn/config.lisp`
  - Windows: `%LOCALAPPDATA%\fn\config.lisp`
- Python 3
  - Unix: `$XDG_CONFIG_HOME/fn/config.py` or `$HOME/.config/fn/config.py`
  - Windows: `%LOCALAPPDATA%\fn\config.py`
- Node.js
  - Unix: `$XDG_CONFIG_HOME/fn/config.js` or `$HOME/.config/fn/config.js`
  - Windows: `%LOCALAPPDATA%\fn\config.js`

## Third-Party Libraries

Add your load statements to the configuration file.

Alternatively, for Common Lisp, add the desired library to `fn.asd` > `:depends-on`, then recompile and reinstall.

Alternatively, for Python 3, import the desired library in `fn.py`, then reinstall.

## Demonstration

Finding the difference between two lists of numbers.

Command:
```sh
( \
  echo 1 2 3 4 5 | fn-lisp -iw '(str:split " " %) (mapcar `read-from-string %)'; \
  echo 2 3 4 5 6 | fn-lisp -iw '(str:split " " %) (mapcar `read-from-string %)' \
) | fn-lisp -ro '(mapcar `- % %) (format nil "~{~a ~}" %)'

( \
  echo 1 2 3 4 5 | fn-py -iw 'P().split()' 'list(map(int, P()))'; \
  echo 2 3 4 5 6 | fn-py -iw 'P().split()' 'list(map(int, P()))' \
) | fn-py -r '[a - b for a, b in zip(P(), P())]' 'print(*P())'
```

Output:
```
1 1 1 1 1
1 1 1 1 1
```
