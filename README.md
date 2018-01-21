# ccomp

A toy C compiler written in Haskell.

This follows the great tutorial from [Nora Sandler](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

## Overview

This compiler compiles to assembly only. It doesn't produce executables nor object files. Thus, its output is similar to GCC's -S flag, i.e :

    gcc -S FILE

## Usage

To compile a C file to assembly, type

    ccomp input.c

The output assembly is printed to `stdout`. Redirect it to a file with

    ccomp input.c > output.s

## Compare with GCC's -S output

To check whether the compiler produced a similar output :

    gcc -S input.c # produces input.s
    ccomp input.c > output.s