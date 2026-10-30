# hssbpl

hssbpl is a compiler for SBPL, a small, statically typed, stack-based programming language. The compiler is written in Haskell and compiles `.sbpl` source files into NASM assembly for Linux x86-64. NASM and `ld` then produce a native executable without a C runtime.

SBPL uses postfix notation: values are pushed onto a data stack, and operators and function calls consume their arguments from that stack. The language supports functions with declared stack effects, local and global bindings, typed pointers, arrays, conditionals, loops, and a simple source preprocessor. Its standard library is itself written in SBPL.

## Requirements and build

- Linux on x86-64
- GHC, including the Haskell `split` package
- NASM and GNU `ld` (binutils)
- Python 3, to run the test suite

Clone the repository and build the compiler from its root directory:

```sh
git clone https://github.com/omyyys/hssbpl.git
cd hssbpl
./build.sh
```

This invokes GHC on `src/Main.hs` and produces `./hssbpl`.

**Standard-library path:** `src/Lexer.hs` currently contains a machine-specific include search path:

```haskell
includeSearchPaths = [ "/home/user/hssbpl/std" ]
```

Change it to the absolute path of `std/` in your checkout, then rerun `./build.sh`. Otherwise programs in `tests/` and `examples/` that use `include "stdio.sbpl"` or other library imports may fail to find those files. Includes are also searched relative to the input source file's directory.

## Compiling and running programs

```sh
./hssbpl tests/hello_world.sbpl
./tests/hello_world
```

By default the compiler writes an assembly file (`tests/hello_world.asm` in this example), assembles it to an object file, links an executable next to the input, and removes the object file.

To compile and immediately execute a program:

```sh
./hssbpl --run tests/hello_world.sbpl
```

The command-line options are:

| Option | Effect |
| --- | --- |
| `-r`, `--run` | Run the generated executable after compilation. |
| `-s`, `--silent` | Suppress compiler status messages, not program output. |
| `--no-asm` | Remove the generated `.asm` file after linking. |
| `--unsafe` | Skip the type-checking pass. |

Exactly one input file is required. Output names are derived from the input path by removing everything from its **first** `.` onward, so avoid additional dots in source paths. The compiler uses NASM's `elf64` format and links with `ld`, rather than targeting another operating system or architecture.

## SBPL basics

Here is a complete program:

```sbpl
include "stdio.sbpl"

func square int64 : int64 in
    dup *
end

5 square puti64
```

It prints `25`. `5` pushes a value, `square` calls a function, and `puti64` prints and consumes the result. There are no argument lists or expression parentheses: `3 4 +` means `3 + 4`, and `3 4 + 2 *` means `(3 + 4) * 2`. Words and operators should be separated by whitespace. A semicolon begins a comment extending to the end of the line.

### Values, types, and operators

The core types are `int64`, `float64`, `char`, and pointers written `ptr@T`, including nested forms such as `ptr@ptr@char`. `ptr@any` is an opaque pointer type; cast it to a concrete pointer type before dereferencing it. String literals are null-terminated character arrays and evaluate to character pointers. `bool` (`int64`), `string` (`ptr@char`), `true`, `false`, and `NULL` are definitions supplied by `std/prelude.sbpl`, not separate built-in types or literals.

Arithmetic (`+`, `-`, `*`, `/`, `%`), comparisons (`=`, `!=`, `<`, `<=`, `>`, `>=`), bitwise operations (`&`, `|`, `~`, `<<`, `>>`), and logical operations (`!`, `&&`, `||`) use postfix notation. Arithmetic and comparisons work with supported `int64` or `float64` operands; `%` and the bitwise operators operate on integers. `int64`, `float64`, `char`, and `ptr@T` are also used as casts. Pointer arithmetic scales offsets by the pointed-to type's size.

Common stack operators are `dup` (duplicate the top value), `drop` (discard it), `swap` (exchange the top two), `over` (copy the second value to the top), and `reach` (move the third value to the top). The built-in `print` prints an integer followed by a newline; the library provides other output functions.

### Bindings and memory

`let` consumes stack values and binds them to names. Names can be rebound with another `let`:

```sbpl
5 6 let x y
x y + print       ; 11

10 let x
x print          ; 10
```

`var name T` reserves one object of type `T`; `declare name T count` reserves an array of `count` such objects. Referencing either name pushes its address (`ptr@T`). The count is a compile-time postfix integer expression, which may use constants expanded by `define`.

`??` reads through a typed pointer and `!!` stores a value through it. The standard prelude provides indexed helpers such as `int64??`, `int64$$`, `char??`, and `char$$`:

```sbpl
include "prelude.sbpl"

declare numbers int64 4
numbers 0 42 int64$$
numbers 0 int64?? print   ; 42
```

### Functions and control flow

A function declares the types it consumes before `:` and the types it leaves on the stack after it. Either side can be empty:

```sbpl
func add_three int64 : int64 in
    3 +
end

func greet : in
    "Hello" puts
end
```

A function is invoked by writing its name. Functions can call one another recursively. `return` exits a function early. The type checker tracks stack types through operations, validates function inputs and outputs, and requires compatible stack effects across control-flow branches and loops. `--unsafe` bypasses these checks; it does not make invalid code safe.

Conditions consume the value produced before `then` or `do`. Zero is false; nonzero is true:

```sbpl
10 let n

while n 0 > do
    n print
    n 1 - let n
end

if n 0 = then
    "Finished" puts
else
    "Unexpected value" puts
end
```

### Includes and definitions

`include "file.sbpl"` inserts another source file during lexing, and repeated includes of the same resolved file are skipped. `define NAME replacement` creates a textual macro expanded by the lexer; definitions can refer to other definitions:

```sbpl
define SIZE 8
define DOUBLE dup +

SIZE DOUBLE print   ; 16
```

`include` and `define` are preprocessing directives, not runtime operations. The standard library uses them for type aliases, constants, and short utility expressions.

## Standard library

All modules are in `std/` and can be loaded with `include` once the include search path is configured.

| Module | Contents |
| --- | --- |
| `linux.sbpl` | Wrappers for Linux system calls (I/O, processes, memory, time). |
| `prelude.sbpl` | Common aliases and constants, typed memory access, character predicates, process and time helpers. |
| `string.sbpl` | Memory and C-style string operations, searching, copying, and integer conversion. |
| `stdio.sbpl` | Console/file I/O and printing integers, floats, characters, and strings. |
| `malloc.sbpl` | `malloc`, `calloc`, `realloc`, and `free` |
| `math.sbpl` | Mathematical constants and functions, including trigonometry, powers, and logarithms. |
| `random.sbpl` | Seeded pseudorandom integers and floating-point values. |
| `complex.sbpl` | Heap-allocated complex values and arithmetic. |

These modules are implementations in SBPL rather than bindings to libc. Higher-level modules include their own dependencies, so most programs only need to include the library modules they use directly.

## Examples and tests

`examples/` contains larger programs demonstrating the language.

Compile and run an example in the same way as any other source file, for instance:

```sh
./hssbpl -r examples/rule110.sbpl
```

The `tests/` directory contains 23 `.sbpl` programs and matching `.txt` files with expected standard output. After building the compiler and configuring the standard-library path, run:

```sh
python3 test.py -f tests
```