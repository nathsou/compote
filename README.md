# A C compiler

Following [Writing a C Compiler by Nora Sandler](https://nostarch.com/writing-c-compiler)

## Supported targets

- x86_64 (Linux, MacOS)

## Usage

- Build the compiler:

```bash
./buid.sh
```

- Compile a C file:

```bash
./target/release/compote input.c
```

- Run the tests

```bash
cd writing-a-c-compiler-tests
./test_compiler ../target/release/compote --chapter 12 --bitwise --compound
```
