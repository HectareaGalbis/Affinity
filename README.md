
# more-cffi

This project can be seen as an extension of the cffi project. Specifically, this defines some macros and symbols to create documented bindings.

## Description

Sometimes wrapping a library is difficult and the way how things must be done is not always clear. This library will help you to define good and documented bindings writing just the necessary information. Also, it tries to create said bindings with the least possible transformations between Lisp and C as possible. In fact, the user will use raw pointers when creating a struct (they won't know). However, this has a counterpart: Sometimes, totally escaping the world of C is impossible and we need to deal with it. In C, objects have to be explicitly destroyed and with this library you will too. For that reason, some macros (`with-` macros) come to the rescue to make things a bit easier. The main macros exported by `more-cffi` are:

* `defcfun`: A mix of `cffi:defcfun` and `cffi:foreign-funcall-pointer`.
* `defwith`: Define `with-` macros.
* `def-foreign-constant`: Define constants.
* `def-foreign-constant-function`: Define functions from C function macros.
* `def-foreign-enum`: Define enums.
* `def-foreign-callback-definer`: Define callback definers.
* `def-foreign-function`: Define a function wrapper.
* `def-foreign-struct`: Define a struct wrapper.
* `def-foreign-union`: Define a union wrapper.

Each of these macros writes documentation if you enable it. Finally, this library exports some C functions to manage data allocated by C. For more information, see:

* [Reference API](https://github.com/Hectarea1996/more-cffi/blob/main/API.md)
* [A brief guide](https://github.com/Hectarea1996/more-cffi/blob/main/Guide.md)




