
# Reference API

## Macros

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

### def-foreign-constant-function

```Lisp
(def-foreign-constant-function file foreign-name name args { body }+)

file ::= A stream.
foreign-name ::= A string designator.
name ::= A string designator.
args ::= ( arg* )
arg ::= A symbol.
body ::= A lisp expression.
```

This macro was created to define simple C macro functions. Define a macro with name `name` that receives the arguments `args`. Each `arg` appearing in the `body` expressions will be substituted by the expression stored in said `arg`. The name `foreign-name` is used only in documentation and it refers to the foreign macro function name. If `file` is not `nil` this will write a description of how the macro looks like. The name of the macro is exported.

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

