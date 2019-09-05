# check-bnf
## What is this?
Macro arguments checker.

### Current lisp world
Common Lisp has true macro.
Writing macro means extending compiler i.e. creating new language.

### Issues
Lack of documentations.
Do you want to use LANGUAGE which has no documentation nor BNF?

### Proposal
Check-bnf provides bnf like syntax checker.

It allows you to write macro arguments definition.

It helps the third person to understand your macro.

## Usage

```lisp
(defmacro defun (&whole whole name lambda-list &body body)
  (check-bnf(:whole whole)
    (name (or symbol setf-name))
    (setf-name ((eql setf) name))
    (lambda-list list))
  (list* name lambda-list body))

(defun "name" () :dummy)
;; SYNTAX-ERROR
  Syntax-error in DEFUN
NAME      := [ SYMBOL | SETF-NAME ]
SETF-NAME := ((EQL SETF) NAME)
but "name"
in (DEFUN "name" NIL :DUMMY)
```

For detail, see spec/check-bnf.lisp

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with
SBCL/1.5.5
CCL/1.11.5

### Test fails with
CLISP due to TRIVIA fails to load.

## Installation

