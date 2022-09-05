
# The more-cffi guide

## Dealing with this guide

This project extends the functionality of cffi in a very specific way. In fact, some other functionality like complex types traslation is not used. Imagine that you need to create the traslation of a complex structure and you use list or whatever. Each time you receive or use that object a traslation needs to be done and maybe you only have used one or two members. That is one of the main reasons I've created this project. Using more-cffi the user of your projects will use raw pointers almost all the time (they will not note it) and the traslations are made per-member and not per-object.

I was making this project as it goes. I think you can solve right now almost every problem that can show up when facing with C code. Despite that, I created this guide to explain how to avoid that problems at the same time I show how to use more-cffi.

Let's start!

## Dealing with documentation

The first you should know is that more-cffi can write some documentation automatically. Usually a C library has already its own documentation so you only need to tell the user how a function or a struct looks like in Common Lisp. Each macro that defines a struct or a function will gather the information you provide and write it in a formatted way. The default output format is markdown (GitHub markdow), although it can be changed.

When creating a project, it is usual to have multiple files where to write the bindings. Using more-cffi, each file would be one documentation page. That file could look like this:

```Lisp
(in-package :my-pkg)

(mcffi:with-doc-file (doc-file "path/to/file.md")

  ;; more-cffi stuff here...

  )
```

Using `with-doc-file` We specify a stream where to store the documentation. You can create your own streams using whatever you want, but more-cffi exports the `with-doc-file` macro. It is like `with-open-file` but simpler. In this example a symbol `doc-file` is bound to the file placed at `path/to/file.md`. Well, it would do that if we enable the generation. By default, the documentation generation is disabled and all the macros exported by more-cffi will only define their things without writing anything. We don't want the documentation to be generated every time the project is loaded.

When your project is ready to load, you can generate the documentation changing the value of `*doc-generation*` that defaults to `nil`. First, load more-cffi:

```Lisp
CL-USER> (asdf:load-system :more-cffi)
```

Now the symbol `*doc-generation*` is exported. Now load your project with its value set to any non-`nil` value:

```Lisp
CL-USER> (let ((mcffi:*doc-generation* t))
           (asdf:load-system :my-project :force t)) ; :force is important!
```

Make sure that all your files are processed using the `:force` option. After this, all the documentation is generated. Each time you do this, the documentation is regenerated.

## Dealing with your project structure

In this guide I will use as example a little portion of the [Common Vulkan](https://github.com/Hectarea1996/common-vulkan) project. The Vulkan library has a lot of different situations that can be problematic if we don't do the right things. The main directories of Common Vulkan are `vulkan/` and `src/`. In the former we will write the cffi bindings. And in the latter we will use more-cffi to write the wrappers and documentation.

## Dealing with cffi bindings

I won't explain how cffi works so this will be short. In the `vulkan/` directory we encounter three files:
*. `cfunctions.lisp`: Where function bindings are placed.
*. `ctypes.lisp`: Where all structs, unions, typedefs... are placed.
*. `load-vulkan.lisp`: Where the foreign library is loaded.

The third file is the first that we need to load. We need to use `define-foreign-library`. It should look like this:

```Lisp
(cffi:define-foreign-library vulkan-loader
  (:linux "libvulkan.so.1")
  (:darwin "libvulkan.1.dylib")
  (:windows "vulkan-1.dll"))

(cffi:use-foreign-library vulkan-loader)
```

Now we are ready to define the cffi bindings. As I said, in `cfunction.lisp` we can see the raw function bindings. They look like this:

```Lisp
(more-cffi:defcfun ("vkCreateInstance" vkcreateinstance
                    funcall-vkcreateinstance)
    vkresult
  (pcreateinfo :pointer)
  (pallocator :pointer)
  (pinstance :pointer))
```

Or this:

```Lisp
(more-cffi:defcfun ("vkGetPhysicalDeviceImageFormatProperties"
                    vkgetphysicaldeviceimageformatproperties
                    funcall-vkgetphysicaldeviceimageformatproperties)
    vkresult
  (physicaldevice vkphysicaldevice)
  (format vkformat)
  (type vkimagetype)
  (tiling vkimagetiling)
  (usage vkimageusageflags)
  (flags vkimagecreateflags)
  (pimageformatproperties :pointer))
```

Nothing special for now. The last file in this directory is `ctypes.lisp` where are stored all the type bindings. We can see typedefs:

```Lisp
(cffi:defctype vkbool32 :uint32)
```

Structs:

```Lisp
(cffi:defcstruct vkbuffermemorybarrier
  (stype vkstructuretype)
  (pnext :pointer)
  (srcaccessmask vkaccessflags)
  (dstaccessmask vkaccessflags)
  (srcqueuefamilyindex :uint32)
  (dstqueuefamilyindex :uint32)
  (buffer vkbuffer)
  (offset vkdevicesize)
  (size vkdevicesize))
```

And unions too:

```Lisp
(cffi:defcunion vkaccelerationstructuregeometrydatakhr
  (triangles (:struct vkaccelerationstructuregeometrytrianglesdatakhr))
  (aabbs (:struct vkaccelerationstructuregeometryaabbsdatakhr))
  (instances (:struct vkaccelerationstructuregeometryinstancesdatakhr)))
```

Writing the raw bindings is the first step to create our project. Once this is done we can start creating the wrappers and documentation.

## Dealing with more-cffi

Let's begin with the interesting part. The `src/` directory contains several files. In the end each file will generate one documentation page. For example, the file `constants.lisp` contains all the constants definitions and when the documentation is generated a file `constants.md` will be created. The same occurs with the rest of the files. The most important files here are:
* `constants.lisp`
* `enums.lisp`
* `functions.lisp`
* `structs.lisp`
* `callbacks.lisp`

