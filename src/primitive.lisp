
(in-package #:affinity)
(in-readtable affinity)


(defmacro define-basic-primitive-types (&rest types)
  `(progn
     ,@(mapcar #¿`(define-primitive-affi-type ,?type () ,?type) types)))

(define-basic-primitive-types
    :char :unsigned-char :short :unsigned-short :int :unsigned-int :long :unsigned-long :long-long
  :unsigned-long-long :uchar :ushort :uint :ulong :llong :ullong :int8 :uint8 :int16 :uint16 :int32 :uint32
  :int64 :uint64 :size :ssize :intptr :uintptr :ptrdiff :offset :float :double :long-double :void)

(define-primitive-affi-type :bool (&optional (base-type :int))
  `(:boolean ,(affi-to-cffi base-type)))
