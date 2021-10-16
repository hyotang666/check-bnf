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
* SBCL/2.0.9
* CCL/1.12
* ECL/20.4.24 ; Failed.
* CLISP/2.49

### Known issue.
#### CLISP
[CLISP say](https://clisp.sourceforge.io/impnotes.html#clpp)

> The Lisp Pretty Printer implementation is not perfect yet.

CHECK-BNF works fine but the printed message is a little bit strange in clisp.
For details [see spec file](spec/check-bnf.lisp).

#### ECL
Currently CHECK-BNF stop to support ECL due to [ECL specific issue](https://gitlab.com/embeddable-common-lisp/ecl/-/issues/570).

#### allegro
Currently CHECK-BNF stop to support allegro due to allegro specific issue.
Here are the session.

```lisp
CL-USER(0): (lambda (x) (typep x '(and atom (not list))))
#<Interpreted Function (unnamed) @ #x2255447a>
CHECK-BNF(1): (funcall * nil)
NIL ; <--- FINE!
CL-USER(2): (compile nil **)
#<Function (:ANONYMOUS-LAMBDA 69) @ #x22557032>
NIL
NIL
CL-USER(3): (funcall * nil)
T ; <--- WTF!?
```

## Installation

quicklisp supported.

```lisp
* (ql:quickload :check-bnf)
```
