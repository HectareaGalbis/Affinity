<h1 id="header:ADP:HEADERTAG2">More-cffi API</h1>

<h4 id="function:MORE-CFFI:DEFINE-CALLBACK-DEFINER">Macro: DEFINE-CALLBACK-DEFINER</h4>

```Lisp
(DEFMACRO MCFFI:DEFINE-CALLBACK-DEFINER (NAME &BODY ARG-DESCRIPTORS)
  ...)
```

````
Define a macro named NAME to define callbacks. This macro has the following syntax:

  (DEFINE-CALLBACK-DEFINER name [docstring] arg-descriptor*)
  docstring      ::= string
  arg-descriptor ::= (slot-name slot-option*)
  slot-name      ::= symbol
  slot-oprion    ::= { :RECEIVER expr } | { :RETURNER expr } | { :TYPE type }1 | { :VIRTUAL expr }

To understand how this macro works we need to talk about C-arguments and Lisp-arguments. If you define a callback using CFFI:DEFCALLBACK
the resulting function will receive C-args. That arguments should be translated to the Lisp world resulting on Lisp-arguments. Regarding
the result value it will be first on the Lisp world and it should be translated to C. With this macro we can establish what are these
arguments. 

If you add an arg-descriptor you are indicating that your callback will have a C-arg named slot-name. Using :TYPE gives to that arg the
specified foreign type (CFFI type). Right now a Lisp-argument is created but no translation will be done. To do a custom translation you must use the :RECEIVER
option. The associated expression must use slot-name and return the new Lisp-arg. But, if the expression is NIL no Lisp-argument will be created.
In this case you should use this C-arg in the :RECEIVER expression of another arg-descriptor. This will result in a callback with one less argument. 

You can use :RETURNER instead of :RECEIVER to indicate slot-name is not a callback argument but a return value. The expression must use
the symbol slot-name to return the new C returned value. 

The last available option is :VIRTUAL. Using this option indicates that slot-name is not a C-arg but will be a Lisp-arg. You should use
:RECEIVER and an expression using the rest of slot-names to initialize this Lisp-arg.
````

<h4 id="function:MORE-CFFI:DEFINE-FOREIGN-STRUCT">Macro: DEFINE-FOREIGN-STRUCT</h4>

```Lisp
(DEFMACRO MCFFI:DEFINE-FOREIGN-STRUCT (STRUCT-TYPE INFIX OPTIONS &BODY
                                       SLOT-DESCRIPTORS)
  ...)
```

````
This macro has the following syntax:

  (DEFINE-FOREIGN-STRUCT struct-type infix-names options [constructor-docstring] [destructor-docstring] slot-descriptor*)

  struct-type           ::= ( { :STRUCT | :UNION } cffi-type )
  cffi-type             ::= 'symbol'
  infix-names           ::= infix-name | (infix-name+)
  infix-name            ::= 'symbol'
  options               ::= option*
  option                ::= { :NO-CONSTRUCTOR | :NO-DESTRUCTOR | :DEFAULT-CONSTRUCTORS | :DEFAULT-READERS | :DEFAULT-WRITERS | :INCLUDE-INVISIBLES }
  constructor-docstring ::= (:CONSTRUCTOR-DOCUMENTATION docstring)
  destructor-docstring  ::= (:DESTRUCTOR-DOCUMENTATION docstring)
  slot-descriptor       ::= slot-name | (slot-name slot-option*)
  slot-name             ::= 'symbol'
  slot-option           ::= { :NAME name } | { :POINTER pointer } | { :VIRTUAL virtual } | { :INITFORM initform } | { :CONSTRUCTOR constructor } | { :DESTRUCTOR destructor } 
                                           | { :READER reader } | { :READER-DOCUMENTATION docstring } | { :WRITER writer } | { :WRITER-DOCUMENTATION docstring }
  name                  ::= 'symbol'
  pointer               ::= 'form'
  virtual               ::= 'form'
  initform              ::= 'form'
  constructor           ::= NIL | (([constructor-arg]) constructor-body*)
  constructor-arg       ::= 'symbol'
  constructor-body      ::= 'form'
  destructor            ::= 'form'
  reader                ::= NIL | (reader-lambda-list reader-body*)
  reader-lambda-list    ::= 'lambda-list'
  reader-body           ::= 'form'
  writer                ::= (writer-lambda-list writer-body*)
  writer-lambda-list    ::= (new-value . rest-lambda-list)
  new-value             ::= 'symbol'
  rest-lambda-list      ::= 'lambda-list'
  docstring             ::= 'string'
  
Define a wraper around the foreign struct (or union) STRUCT-TYPE. Each INFIX-NAME is used to generate the function names.
In fact, for each INFIX-NAME, a constructor, destructor and set of accessors are defined. You should use this if multiple C types
refer to the same struct type.

OPTIONS is a list of keywords that modify the way this macro works. If :NO-CONSTRUCTOR is specified, then this macro will 
not define a constructor. In the same way, if :no-destructor is specified no destructor will be defined. If :DEFAULT-CONSTRUCTORS
 is used each slot member has no need to define an explicit constructor. Instead, a default initialization of the foreign slot is 
done (no translation). The same occurs with :DEFAULT-READERS and :DEFAULT-WRITERS when defining the accessors. You will need 
to write every slot you want to define a constructor, destructor, reader or writer for. However, if :INCLUDE-INVISIBLES is used 
that functions are defined even if you don't specify the existence of that slot.

Using CONSTRUCTOR-DOCSTRING and DESTRUCTOR-DOCSTRING adds a docstring to the constructor and destructor respectively.

Each SLOT-DESCRIPTOR specify the translation of a foreign struct member. The SLOT-NAME is a symbol denoting a foreign member of 
the struct STRUCT-TYPE (unless :VIRTUAL is used). The different options allow you to specify the translations of each member:

  - NAME:        If used, that symbol will be used to create the accessors names.

  - INITFORM:    Specifies the default value of SLOT-NAME member.
 
  - CONSTRUCTOR: This option is used to specify a translation when receiving a parameter in the constructor. If the expression after :CONSTRUCTOR
                 is NIL or neither this option nor :DEFAULT-CONSTRUCTORS are used a constructor will not be defined. If this option is not used
                 but :DEFAULT-CONSTRUCTORS is, a parameter is still received to initialize the foreign member but no translation will be performed.
                 The expression after :CONSTRUCTOR, if not NIL, must be a list where the first element is NIL or a list with a symbol. If NIL,
                 no argument will be received in the constructor. Otherwise, an argument with name the specified symbol will be received. In both cases,
                 the rest of elements of the list after :CONSTRUCTOR must be expressions to initialize the member SLOT-NAME (using setf, for example).
                 It is possible to use the arguments specified from other :CONSTRUCTOR options. Also, every SLOT-NAME is accessible from here.

  - DESTRUCTOR:  This must be an expression doing something with SLOT-NAME, like CFFI:FREE-ing it.
  - READER:      This option works the same as :CONSTRUCTOR about when an accessor is defined or not. Depending on the expression after :READER and
                 :DEFAULT-READERS the accessor may be defined or not. If the expression after :READER is not NIL, then it must be a list where the first
                 element is the lambda list of the accessor. The rest of elements are forms that must return the desired value. Every SLOT-NAME is accessible.
  
  - WRITER:      Same as :READER but indicates that the accessor is SETF-able. The same rules apply here when using or not :DEFAULT-WRITERS.
                 The expression after :WRITER, if not NIL, must be a list where its first element is the lambda list of the SETF function. So, a first
                 symbol is required always denoting the NEW-VALUE. The rest of elements are forms that should modify SLOT-NAME. Every SLOT-NAME is accessible.
  
  - POINTER:     Some structs have as a member another struct. You may want to use its pointer rather than the struct itself. To do that you only need to
                 use a non-NIL expression after :POINTER.
  
  - VIRTUAL:     Using :VIRTUAL you can receive an additional parameter in the constructor, or create additional accessors.

An additional note: If :NO-CONSTRUCTOR and :NO-DESTRUCTOR are NOT used, then a 'with-constructor' is defined. See the Clith project for more information.
````

