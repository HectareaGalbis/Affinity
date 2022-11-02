# More-cffi

This project defines some macros that help you to create wrappers to foreign functions, callbacks and structs. Also, it supports [ADP](https://github.com/Hectarea1996/adp) to generate automatic documentation.

* [More-cffi](/README.md#more-cffi)
  * [Description](/README.md#description)
* [More-cffi API](/docs/mcffi-api.md#more-cffi-api)

## Description

Wrapping a foreign library using CFFI is difficult and the way how things must be done is not always clear. This project exports some utility macros to define such wrappers.

* D
  * [MCFFI:DEFCFUN](/docs/mcffi-api.md#macro-defcfun)
  * [MCFFI:DEFINE-FOREIGN-CALLBACK-DEFINER](/docs/mcffi-api.md#macro-define-foreign-callback-definer)
  * [MCFFI:DEFINE-FOREIGN-FUNCTION](/docs/mcffi-api.md#macro-define-foreign-function)
  * [MCFFI:DEFINE-FOREIGN-STRUCT](/docs/mcffi-api.md#macro-define-foreign-struct)
  * [MCFFI:DEFWITH](/docs/mcffi-api.md#macro-defwith)

