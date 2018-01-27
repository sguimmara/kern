# kern

A C89 (or ANSI C) compiler written in Haskell.

## Overview

kern compiles C89 code into [AT&T assembly](http://www.imada.sdu.dk/Courses/DM18/Litteratur/IntelnATT.htm).

kern is not :
* A preprocessor
* An object code generator
* A linker

## Usage

To compile a C file to assembly, type

    kern input.c

The output assembly is printed to `stdout`. Redirect it to a file with

    kern input.c > output.s

For example :

`returnparam.c`
```c
int main(int x, int y)
{
    return y;
}
```

compiles to

```assembly
    .file   "returnparam.c"
    .globl  main
    .type   main, @function
main:
    pushq   %rbp
    movq    %rsp, %rbp
    movq    %rbp, %rsp
    movl    %esi, %eax
    popq    %rbp
    ret
    .ident  "kern"
```

## Details

* Outputs [AT&T assembly](http://www.imada.sdu.dk/Courses/DM18/Litteratur/IntelnATT.htm)
* Follows the [X86-64 System V ABI](https://software.intel.com/sites/default/files/article/402129/mpx-linux64-abi.pdf)

## Reference
* [Writing a C compiler ](https://norasandler.com/2017/11/29/Write-a-Compiler.html) from Nora Sandler
* [The ANSI C syntax](https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm)