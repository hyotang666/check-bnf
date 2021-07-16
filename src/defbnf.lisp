(in-package :check-bnf)

(declaim (optimize speed))

;;;; NATIVE SUPPORTS.

(defbnf
  ((<lambda-list> (lambda-elt*))
   (lambda-elt (or var init-form))
   (var (and symbol (not (or boolean keyword))))
   (init-form ((or symbol external-spec) expression suppliedp?))
   (external-spec (symbol var))
   (suppliedp? var)))

(defbnf
  ((<function-type>
     (or (eql function) ((eql function) lambda-list-spec return-spec)))
   (lambda-list-spec (or (member * t) (lambda-elt-spec*)))
   (lambda-elt-spec
    (or type-specifier
        (member &rest &optional &key &allow-other-keys)
        spec-&key))
   (spec-&key (keyword type-specifier))
   (return-spec (or type-specifier values-spec))
   (values-spec ((eql values) values-elt*))
   (values-elt (or (member &optional &rest) type-specifier))))

(defbnf
  ((<declaration> ((eql declare) decl-spec*))
   (decl-spec*
    (or spec-special
        spec-type
        spec-ftype
        spec-inline
        spec-notinline
        spec-ignore
        spec-optimize
        spec-declaration
        spec-dynamic-extent))
   (spec-special ((eql special) var+))
   (var symbol)
   (spec-type ((eql type) type-specifier var+))
   (spec-ftype ((eql ftype) <function-type> function-name+))
   (function-name (or symbol ((eql setf) symbol)))
   (spec-inline ((eql inline) function-name+))
   (spec-notinline ((eql notinline) function-name+))
   (spec-ignore ((eql ignore) var+))
   (spec-optimize ((eql optimize) quality-spec+))
   (quality-spec (or quality-name (quality-name (integer 0 *))))
   (quality-name (member speed space safety compilation-speed t))
   (spec-declaration ((eql declaration) symbol+))
   (spec-dynamic-extent ((eql dynamic-extent) var+))))