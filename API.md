
# Reference API

## Macros

### def-lisp-function

```Lisp
(def-lisp-function file name args body )

file ::= A stream.
name ::= A name designator.
args ::= (arg)*
arg ::= A symbol.
body ::= { body-expr body } | { type-declaration body-tail }
body-tail ::= { body-expr }*
body-expr ::= A Lisp expression.
type-declaration ::= (declare-types arg-decls [:return return-decls])
arg-decls ::= (arg-type { arg-name }+)
arg-type ::= A Lisp form.
arg-name ::= A string designator.
return-decls ::= (ret-type { ret-name }+)
ret-type ::= A lisp form.
ret-name ::= A string designator.
```

Same as `defun`, but it must have a `declare-types` expression that contains the types of the received and returned values. That expression does nothing, it is used for documentation. The docstring is used too for documentation.

### def-lisp-macro

```Lisp
(def-lisp-macro file name args body)

file ::= A stream.
name ::= A string designator.
args ::= {arg}*
arg ::= A symbol.
body ::= { body-expr }*
body-expr ::= A Lisp expression.
```

Same as `defmacro`. It uses the docstring for documentation. 

### defcfun

```Lisp
(defcfun (foreign-name name [funcall-name]) return-type arguments)

foreign-name ::= A string designating the foreign function.
name ::= A symbol designating the name of the resulting lisp function or nil.
funcall-name ::= A symbol designating the name of the resulting lisp function that accepts, additionally, a function pointer. The default value is nil.
return-type ::= A foreign type designating the return type of the function.
arguments ::= { (arg-name arg-type) }*
arg-name ::= A symbol.
arg-type ::= A foreign type.
```

When `name` is not `nil`, define a lisp function that calls the foreign function with name `foreign-name`. This works similar to `defcfun` from cffi.

When `funcall-name` is not `nil`, define a lisp function that works as `name`, but accepts additionally a pointer to the foreign function.


### defwith

```Lisp
(defwith file name create destroy [destructor-options])

file ::= A stream or nil.
name ::= A string designator.
create ::= A string designator.
destroy ::= A string designator.
destructor-options ::= { destructor-option }*
destructor-option ::= { :destructor-arity num } | { :destructor-arguments (num*) }
num ::= A non-negative integer.
```

Define a `with-` macro with name `name` and export it. The resulting macro will wrap an expression with `create` and `destroy` functions. With the `destructor-options` you can control how and how many arguments will receive the `destroy` function. If `:destructor-arity` is specified, then `destroy` will accept the first `num` arguments returned by `create`. If `:destructor-arguments` is specified, you can control the order in which the arguments are passed to `destroy`. For example, if `:destructor-arguments (2 0 3)` is used, `destroy` will receive the third, first and fourth arguments from `create` and in that order. Finally, if `file` is not `nil` this will write a simple description of what the macro does.

### def-foreign-constant

```Lisp
(def-foreign-constant file foreign-name name value)

file ::= A stream.
foreign-name ::= A string designator.
name ::= A string designator.
value ::= A lisp value.
```

Define a constant using `defparameter` with name `name` and value `value`. The `foreign-name` is used only for documentation and it refers to the foreign constant name. If `file` is not `nil`, this will write a simple description of the defined constant.

### def-foreign-enum

```Lisp
(def-foreign-enum file name slots)

file ::= A stream.
name ::= A string designator.
slots ::= { slot }*
slot ::= (slot-name slot-value)
slot-name ::= A string designator.
slot-value ::= A lisp value.
```

First, define a cffi foreign type with name `name` that is equivalent to `:int`. Afterwards, define a constant with name `slot-name` and value `slot-value` for each `slot`. If `file` is not `nil`, this will write a description of this enum. Each of the defined constants are exported.

### def-foreign-callback-definer

```Lisp
(def-foreign-callback-definer file foreign-type name arg-descriptors)

file ::= A stream.
foreign-type ::= A string designator.
name ::= A string designator.
arg-descriptors ::= { arg-descriptor }*
arg-descriptor ::= (arg-name arg-options)
arg-name ::= A string designator.
arg-options ::= { arg-types [create-or-return] } | { [create-or-return] arg-types }
arg-types ::= { :type arg-type :foreign-type foreign-arg-type } | { :foreign-type foreign-arg-type :type arg-type }
arg-type ::= A lisp form.
foreign-arg-type ::= A cffi foreign type.
create-or-return ::= { :create expr } | { :return expr }
expr ::= A lisp expression.
```

Define a callback definer. The name `foreign-type` specifies the possible C name of the callback (only used for documentation). The new callback definer will have the name `name`. Each `arg-descriptor` describes a traslation of the args passed to the callback. `foreign-arg-type` is the C type of the argument and `arg-type` is the lisp type (the latter is used only for documentation). With `:create` you are specifying that the argument is being received by the callback and the expression is a C to Lisp traslation (the expression must return the traslated object). All these expressions have access to every argument. The `:return` option can be specified just once. In that case, the `arg-name` is bound to the callback result and the expression must be a Lisp to C traslation (the expression must return the traslated object). `:return` and `:create` cannot appear in the same `arg-descriptor`.

### def-foreign-function

```Lisp
(def-foreign-function file (foreign-name name [funcall-name]) args body)

foreign-name ::= A string designator.
name ::= A string designator or nil.
funcall-name ::= A string designator or nil.
args ::= { arg }*
arg ::= A symbol.
body ::= { body-expr body } | { type-declaration body-tail }
body-expr ::= A Lisp expression.
type-declaration ::= (declare-types arg-decls [:return return-decls])
arg-decls ::= (arg-type { arg-name }+)
arg-type ::= A Lisp form.
arg-name ::= A string designator.
return-decls ::= (ret-type { ret-name }+)
ret-type ::= A lisp form.
ret-name ::= A string designator.
```

If `name` is not `nil`, define a function that must use `foreign-name` and must contain a `declare-types` expression. If `funcall-name` is not `nil`, then another function is defined. This is the same as the former, but accepts as first argument a pointer to the foreign function with name `foreign-name`. The `declare-types` expression does nothing and is used only for documentation. The docstring of the function is used also for documentation.

### def-foreign-macro

```Lisp
(def-foreign-macro file (foreign-name name) args body)

file ::= A stream.
foreign-name ::= A string-designator
name ::= A string designator.
args ::= {arg}*
arg ::= A symbol.
body ::= { body-expr }*
body-expr ::= A Lisp expression.
```

Same as `def-lisp-macro` but uses `foreign-name` for documentation.

### def-foreign-struct

