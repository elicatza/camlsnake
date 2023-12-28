#!/usr/bin/env bash

set -xe

CFLAGS="-Wall -Wextra -pedantic"
LIBS="-I$(ocamlc -where) $(pkg-config --libs raylib)"

clang -shared -fPIC $CFLAGS ./caml_raylib.c -o dllcaml_raylib.so $LIBS

ocamlc ./raylib.ml ./main.ml -o snake_caml -dllpath . -dllib -lcaml_raylib
