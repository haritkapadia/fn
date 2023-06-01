#!/bin/sh

rm fn && sbcl --load fn.asd --eval '(progn (ql:quickload :fn) (asdf:make :fn) (quit))' && cp fn ~/.local/bin
