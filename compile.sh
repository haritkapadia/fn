#!/bin/sh

if test -f fn-lisp; then
    rm fn-lisp
fi
sbcl --load fn.asd --eval '(progn (ql:quickload :fn) (asdf:make :fn) (quit))'

cp fn.py fn-py
cp fn.js fn-node
