# scmpy

A naive *Scheme* to *Python* transpiler.

## Clone & Build

``` sh
$ git clone https://github.com/Meowcolm024/scmpy.git && cd scmpy
$ stack build
```

## Usage

Compile example file and run output py file:

``` sh
# Notice that the `-f` flag is necessary.
$ stack exec -- scmpy-exe -f example/fib.scm
$ python a.py
89
```

Use the `-h` to see more options:

``` sh
stack exec -- scmpy-exe -h
scmpy - A Scheme to Python transpiler

Usage: scmpy-exe (-f|--file SOURCE) [-o|--output OUTPUT] [--header] [--llist]
  Transpile Scheme to Python

Available options:
  -f,--file SOURCE         scheme source file
  -o,--output OUTPUT       output Python file
  --header                 generate separate file for helper functions
  --llist                  use lisp style lists
  -h,--help                Show this help text
```

- Notice `-llist` is not available yet.

## Issues

- muatble ops like `set!` are not supported.
- vars like `list`, `not`, `and`, `or`, `map`, `filter` are reserved.
