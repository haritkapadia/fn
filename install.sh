#!/bin/sh

sbcl --load fn.asd --eval '(progn (ql:quickload :fn) (asdf:make :fn) (quit))' && cp fn ~/.local/bin
