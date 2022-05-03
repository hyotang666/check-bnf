# CHECK-BNF 8.0.0
## What is this?
Macro arguments checker.

### Current lisp world
Common Lisp has a true macro.
Writing macro means extending the compiler i.e. creating a new language.

### Issues
Lack of documentation.
Do you want to use LANGUAGE which has no documentation nor BNF?

### Proposal
Check-bnf provides a bnf like syntax checker.

It allows you to write macro arguments definition.

It helps the third person to understand your macro.

## Usage

```lisp
(defmacro defun (&whole whole name lambda-list &body body)
  (check-bnf (:whole whole)
    ((name (or symbol setf-name))
     (setf-name ((eql setf) symbol)))
    ((lambda-list list)))
  (list* name lambda-list body))

(defun "name" () :dummy)
;; SYNTAX-ERROR
  Syntax-error in DEFUN

NAME      := [ SYMBOL | SETF-NAME ]
SETF-NAME := ((EQL SETF) SYMBOL)

but "name"

in (DEFUN "name" NIL :DUMMY)
```

For detail, see [spec file](spec/check-bnf.lisp).

## DOC
You may emb bnf as documentation.
Macro `DOC` allows you to do it by read time evaluation and read time labeling.

```lisp
(defmacro your-macro (&whole w a)
  #.(check-bnf:doc "Header for your-macro"
      #0=(check-bnf:check-bnf (:whole w)
           ((a symbol))))
  #0#
  `',a)
=> YOUR-MACRO

(documentation 'your-macro 'function)
=> "Header for your-macro
A := SYMBOL
"
```

## DEFBNF
Your macro may have similar syntax e.g. CASE, CCASE, and ECASE.
In such cases, you can define global BNF by `DEFBNF`.

`DEFBNF` has almost the same syntax as `CHECK-BNF` but except accepts only one definition and alias is invalid.

Currently `<LAMBDA-LIST>`, `<FUNCTION-TYPE>` and `<DECLARATION>` is provided.

## From developer

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with
* SBCL/2.2.4
* CCL/1.12.1
* CLISP/2.49
* CMUCL/21D
* ECL/21.2.1
* Allegro/10.1
* ABCL/1.9.0

### Known issue.
#### CCL
Due to its own [issue](https://github.com/Clozure/ccl/issues/350), ccl signals warnings.
But the check-bnf features are works fine.
In the test, we just muffled it.

#### CLISP
[CLISP say](https://clisp.sourceforge.io/impnotes.html#clpp)

> The Lisp Pretty Printer implementation is not perfect yet.

CHECK-BNF works fine but the printed message is a little bit strange in clisp.
For details [see spec file](spec/check-bnf.lisp).

#### ABCL
CHECK-BNF works fine but the printed message is a little bit strange in abcl
due to [the abcl pretty printing issue](https://github.com/armedbear/abcl/issues/406).

Currently tests about pretty printings are ignored.

## Installation

quicklisp supported.

```lisp
* (ql:quickload :check-bnf)
```
