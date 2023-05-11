# fn

`fn` is a program that executes Common Lisp code provided as arguments. It also has utility arguments for reading and writing to standard input and output as strings or `read`able data.

`fn` is designed to be used ad hoc in a shell.

## Usage

```
fn -[ri] -[wo] code...

-r: Reads standard input as a stream of `read`able data
-i: Reads standard input as a string
-w: Writes stack as `read`able data to standard output
-o: Prints top of stack to standard output
-n: Disables `fn`-defined macros
code: Common Lisp source code. Each returned value is pushed onto a stack. The following macros are available:
  %: Pops top element of stack (fn:s-pop).
  %n: Pops element n from stack (fn:s-pop n).
  $: Gets top element of stack (fn:s-get).
  .: Deletes top element of stack (fn:s-delete).
```

## The Stack

Every top-level expression is evaluated and pushed onto a stack. Combined with the stack manipulation macros `%`, `\$`, and `.`, data can be "piped" between expressions. This enables code to be written more linearly, which is important since text editing and balancing nested parentheses is difficult on most terminals.

```sh
$ fn -w 1 2
1
2

$ fn -w '1 2 (+ % %)'
3

$ fn -w '1 2 (+ $ $)'
1
2
4

$ fn -w '1 2 (+ $0 $1)'
1
2
3
```

## Third-Party Libraries

Add the desired library to `fn.asd` > `:depends-on`, then recompile and reinstall.
