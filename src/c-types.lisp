
(in-package :cxx)

;; extern "C" typedef union {
;;   float Float;
;;   double Double;
;;   long double LongDouble;
;; } ComplexType;
(defcunion complex-type
  (f :float)
  (d :double))

;; ;; extern "C" typedef struct {
;; ;;   ComplexType real;
;; ;;   ComplexType imag;
;; ;; } LispComplex;
(defcstruct cxx-complex
  (real (:union complex-type))
  (imag (:union complex-type)))

;; extern "C" typedef struct {
;;   char *name;
;;   char *super_classes;
;;   char *slot_names;
;;   char *slot_types;
;;   void *constructor;
;;   void *destructor;
;; } ClassInfo;
(defcstruct class-info
  (name :string)
  (super-classes :string)
  (slot-names :string)
  (slot-types :string)
  (constructor :pointer)
  (destructor :pointer))
;; extern "C" typedef struct {
;;   char *name;
;;   bool method_p;
;;   char *class_obj;
;;   void *thunk_ptr;
;;   void *func_ptr;
;;   char *arg_types;
;;   char *return_type;
;; } FunctionInfo;
(defcstruct function-info
  (name :string)
  (method-p :bool)
  (class-obj :string)
  (thunc-ptr :pointer)
  (func-ptr :pointer)
  (arg-types :string)
  (return-type :string))

;; extern "C" typedef struct {
;;   char *name;
;;   char *value;
;; } ConstantInfo;
(defcstruct constant-info
  (name :string)
  (value :string))

;; typedef union {
;;   clcxx::FunctionInfo Func;
;;   clcxx::ClassInfo Class;
;;   clcxx::ConstantInfo Const;
;; } MetaData;
(defcunion Meta-Data
  (func (:struct function-info))
  (class (:struct class-info))
  (const (:struct constant-info)))
