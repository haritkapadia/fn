# fn

`fn` is a program that executes Common Lisp code provided as arguments. It also has utility arguments for reading and writing to standard input and output as strings or `read`able data.

`fn` is designed to be used ad hoc in a shell.

## Usage

```
fn OPTIONS... CODE...

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

## The Stack

Every top-level expression is evaluated and pushed onto a stack. Combined with the stack manipulation macros `%`, `\$`, and `.`, data can be "piped" between expressions. This enables code to be written more linearly, which is important since text editing and balancing nested parentheses is difficult on most terminals.

```sh
$ fn -w 1 3
1
3

$ fn -w '1 3 (+ % %)'
4

$ fn -w '1 3 (+ $ $)'
1
3
6

$ fn -w '1 3 (+ $0 $1)'
1
2
4
```

## Configuration

The following file will be loaded before arguments are executed:

- Unix: `$XDG_CONFIG_HOME/fn/config.lisp` or `$HOME/.config/fn/config.lisp`
- Windows: `%LOCALAPPDATA%\fn\config.lisp`

## Third-Party Libraries

Add your load statements to the configuration file.

Alternatively, add the desired library to `fn.asd` > `:depends-on`, then recompile and reinstall.

## Demonstration

Finding the difference between two lists of numbers.

Command:
```sh
( \
  echo 1 2 3 4 5 | fn -iw '(str:split " " %) (mapcar `read-from-string %)'; \
  echo 2 3 4 5 6 | fn -iw '(str:split " " %) (mapcar `read-from-string %)' \
) | fn -ro '(mapcar `- % %) (format nil "~{~a ~}" %)'
```

Output:
```
1 1 1 1 1
```
