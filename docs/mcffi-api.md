<h1 id="header:ADP:HEADERTAG2">More-cffi API</h1>

<h4 id="function:MORE-CFFI:DEFWITH">Macro: DEFWITH</h4>

```Lisp
(defmacro MCFFI:DEFWITH (NAME CREATE DESTROY &OPTIONAL (ARITY 1) DOCSTRING)
  ...)
```

````
Define a macro named NAME. This new macro has the following syntax:

  (NAME var-or-vars (&rest args) &body body)

When using this new macro CREATE is called with ARGS and the results are stored in VAR-OR-VARS. If VAR-OR-VARS
is a symbol, the rest of returned values are ignored. Afterwards, the BODY forms are evaluated. Finally, 
DESTROY is called. The arguments used by DESTROY depends on ARITY. If ARITY is a list of non-negative integers
then they denote the arguments returned by CREATE to be used by DESTROY in the order they appear. For example, 
using the list (3 0 2) indicates that DESTROY will receive the fourth, first and third values returned by CREATE 
and in that order. If ARITY is just a non-negative integer then it indicates the number of arguments
to be used by DESTROY. For example, if 2 is specified, then DESTROY will receive the first 2 values returned by CREATE.
````

<h4 id="function:MORE-CFFI:DEFINE-CALLBACK-DEFINER">Macro: DEFINE-CALLBACK-DEFINER</h4>

```Lisp
(defmacro MCFFI:DEFINE-CALLBACK-DEFINER (NAME &BODY ARG-DESCRIPTORS)
  ...)
```

````
Define a macro named NAME to define callbacks. Each ARG-DESCRIPTOR must have the following syntax:

  ARG-DESCRIPTOR ::= (SLOT-NAME [[slot-option]])
  SLOT-OPTION    ::= {:create expr | :return expr}1 | {:type type}1 | :virtual expr

To understand how this macro works we need to talk about C-arguments and Lisp-arguments. If you define a callback using CFFI:DEFCALLBACK
the resulting function will receive C-args. That arguments should be translated to the Lisp world resulting on Lisp-arguments. Regarding
the result value it will be first on the Lisp world and it should be translated to C. With this macro we can establish what are these
arguments. 

If you add an ARG-DESCRIPTOR you are indicating that your callback will have a C-arg named SLOT-NAME. Using :TYPE gives to that arg the
specified type. Right now a Lisp-argument is created but no translation will be done. To do a custom translation you must use the :CREATE
option. The associated expression must use SLOT-NAME and return the new value. But, if expression is NIL no Lisp-argument will be created.
In this case you should use this value in the :CREATE expression of another ARG-DESCRIPTOR. In other words, the resulting callbacks will have
one less argument. 

You can use :RETURN instead of :CREATE to indicate SLOT-NAME is not a callback argument but a return value. 

The last available option is :VIRTUAL. Using this option indicates that SLOT-NAME is not a C-arg but will be a Lisp arg. You should use
:CREATE and an expression using the rest of SLOT-NAMEs.
````

<h4 id="function:MORE-CFFI:DEFINE-FOREIGN-STRUCT">Macro: DEFINE-FOREIGN-STRUCT</h4>

```Lisp
(defmacro MCFFI:DEFINE-FOREIGN-STRUCT (STRUCT-TYPE INFIX OPTIONS &BODY
                                       SLOT-DESCRIPTORS)
  ...)
```

````
Define a wraper around the foreign struct (or union) STRUCT-TYPE. The symbol INFIX is used to generate the function names.
OPTIONS is a list of keywords that modify the way this macro works. 

  OPTIONS ::= [[OPTION]]
  OPTION  ::= :no-constructor | :no-destructor | :default-constructors | :default-readers | :default-writers | :include-invisibles

If :no-constructor is specified, then this macro will not define a constructor. In the same way, if :no-destructor is specified
no destructor will be defined. If :default-constructors is used each slot member has no need to define an explicit constructor. Instead,
a default initialization of the foreign slot is done (no translation). The same occurs with :default-readers and :default-writers when
defining the accessors. You will need to write every slot you want to define a constructor, destructor, reader or writer for. However,
if :include-invisibles is used that functions are defined even if you don't specify the existence of that slot.

The SLOT-DESCRIPTIONS specify the translations of each foreign member. The systaxis is as follows:

  SLOT-DESCRIPTIONS ::= (SLOT-NAME [[SLOT-OPTION]])
  SLOT-OPTION       ::= :name NAME | :type TYPE | :pointer POINTER | :virtual VIRTUAL | :constructor CONSTRUCTOR | :destructor DESTRUCTOR | :reader READER | :writer WRITER

The SLOT-NAME is a symbol denoting a foreign member of the struct STRUCT-TYPE (unless :virtual is used). The different options allow you to 
specify the translations of each member:

  - NAME: If used, that symbol will be used to create the accessors names. 
  - TYPE: If used, specify the Lisp type of SLOT-NAME. 
  - CONSTRUCTOR: This option is used to specify a translation when receiving a parameter in the constructor. If the expression after :constructor
                 is NIL or neither this option nor :default-constructors are used a constructor will not be defined. If this option is not used
                 but :default-constructors is, a parameter is still received to initialize the foreign member but no translation will be performed.
                 The expression after :constructor, if not NIL, must be a list where the first element is NIL or a list with a symbol. If NIL,
                 no argument will be received in the constructor. Otherwise, an argument with name the specified symbol will be received. In both cases,
                 the rest of elements of the list after :constructor must be expressions to initialize the member SLOT-NAME (using setf, for example).
                 It is possible to use the arguments specified from other :constructor options. Also, every SLOT-NAME is accessible from here.
  - DESTRUCTOR: This must be an expression doing something with SLOT-NAME, like cffi:free-ing it.
  - READER: This option works the same as :constructor about when an accessor is defined or not. Depending on the expression after :reader and
            :default-readers the accessor may be defined or not. If the expression after :reader is not NIL, then it must be a list where the first
            element is the lambda list of the accessor. The rest of elements are forms that must return the desired value. Every SLOT-NAME is accessible.
  - WRITER: Same as :reader but indicates that the accessor is setf-able. The same rules apply here when using or not :default-writers.
            The expression after :writer, if not NIL, must be a list where its first element is the lambda list of the setf function. So, a first
            symbol is required always denoting the new value. The rest of elements are forms that should modify SLOT-NAME. Every SLOT-NAME is accessible.
  - POINTER: Some structs have as a member another struct. You may want to use its pointer rather than the struct itself. To do that you only need to
             use a non-NIL expression after :pointer.
  - VIRTUAL: Using :virtual you can receive an additional parameter in the constructor, or create additional accessors.
````

