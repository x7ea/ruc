# Ruc

**_Make C more easy to code!_** the low-layer programming language

> [!NOTE]
> Compile for x86_64 Linux PC only (tested on Debian 13)

## Features

- Aligned to the System V ABI, so able to link with the GNU C Library
- Everything is an expression, that returns value (stored in `rax` register)
- No type system! all values are 64-bit integer, you can freely operate it

## Goal

The goal in this project is that optimize for professional Linux computing.\
By original compiler backends, that emits Netwide assembler code directly.

> [!TIP]
> Ruc just does **simplify** C with expression-based syntax\
> In the Functional programming style like OCaml or Rust

Thanks for support Ruc project.
