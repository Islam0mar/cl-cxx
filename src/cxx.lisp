
(in-package :cxx)

;; bool remove_package(char *pack_name)
(defcfun ("remove_package" remove-c-package) :bool
  (name :string))

;; bool clcxx_init(void (*error_handler)(char *),
;;                      void (*reg_data_callback)(MetaData *, uint8_t))
(defcfun ("clcxx_init" clcxx-init) :bool
  (err-callback :pointer)
  (reg-data-callback :pointer))

;; bool register_lisp_package(const char *cl_pack,
;;                                  void (*regfunc)(clcxx::Package &))
(defcfun ("register_package" register-package) :bool
  (name :string)
  (pack-ptr :pointer))

;; size_t used_bytes_size();
(defcfun ("used_bytes_size" number-of-allocated-bytes) :size)

;; size_t max_stack_bytes_size();
(defcfun ("max_stack_bytes_size" max-stack-bytes-size) :size)

;; bool delete_string(char *string);
(defcfun ("delete_string" destruct-string) :bool
  (string-to-be-deleted :string))


(defun symbols-list (arg-types &optional (method-p nil) (class-obj nil))
  "Return a list of symbols '(V0 V1 V2 V3 ...)
   representing the number of args"
  (let ((m-obj (if  method-p `(obj ,(read-from-string  class-obj))))
        (lst (if arg-types (loop for i in (split-string-by arg-types #\+)
                              for j from 0
                              collect (intern (concatenate 'string "V" (write-to-string j)))))))
    (if method-p
        (if lst (append (list m-obj) lst) (list m-obj))
        lst)))

(defun compound-type-list (type &optional (array-p nil))
  "Returns a list of strings representing compound type"
  (declare (type string type))
  (let* ((string (string-trim "(" type))
         (str (get-parenthes-string string))
         (lst (append (split-string-by
                       (string-trim ")"
                                    (remove-string str string))
                       #\space) (list str))))
    (if str (progn (if array-p (rotatef (nth 1 lst) (nth 2 lst)))
                   lst)
        (split-string-by (string-trim ")" string) #\space))))

(defun parse-type (type)
  "Returns cffi-type as a keyword
      or list of keywords"
  (declare (string type))
  (if (equal (subseq type 0 1) "(")
      ;; compound type
      (cond ((if (>= (length type) 7) (equal (subseq type 0 7) "(:array") nil)
             (mapcar #'parse-type (compound-type-list type t)))
            ((if (>= (length type) 9) (equal (subseq type 0 9) "(:pointer") nil)
             (mapcar #'parse-type (compound-type-list type)))
            ((if (>= (length type) 11) (equal (subseq type 0 11) "(:reference") nil)
             (mapcar #'parse-type (compound-type-list type)))
            ((if (>= (length type) 17) (equal (subseq type 0 17) "(:const-reference") nil)
             (mapcar #'parse-type (compound-type-list type)))
            ((if (>= (length type) 9) (equal (subseq type 0 9) "(:complex") nil)
             (mapcar #'parse-type (compound-type-list type)))
            ((if (>= (length type) 8) (equal (subseq type 0 8) "(:struct") nil)
             (mapcar #'parse-type (compound-type-list type)))
            ((if (>= (length type) 7) (equal (subseq type 0 7) "(:class") nil)
             (mapcar #'parse-type (compound-type-list type)))
            (t (error "Unkown type : ~S" type)))
      ;; simple type
      (read-from-string type)))

(defun cffi-type (type)
  "Returns cffi-type as a keyword
      or list of keywords"
  (declare (type string type))
  (let ((parsed-type (parse-type type)))
    (if (listp parsed-type) (ecase (first parsed-type)
                              (:array (append (list (second parsed-type)) (list :count)  (list (third parsed-type))))
                              (:complex :pointer)
                              (:pointer (if (equal (second parsed-type)
                                                   :char)
                                            :string
                                            :pointer))
                              (:reference :pointer)
                              (:const-reference :pointer) ;; TODO:add const
                              (:class :pointer)
                              (:struct (second parsed-type)))
        parsed-type)))


(defun parse-args (arg-types &optional (input-type-p t))
  "return argument types (with variables if they are inputs) in a proper list"
  (if input-type-p (if arg-types (loop
                                    for i in (split-string-by arg-types #\+)
                                    for sym in (symbols-list arg-types)
                                    as type = (cffi-type i) then (cffi-type i)
                                    append
                                      (let* ((parsed-type (parse-type i))
                                             (name (cond
                                                     ;; in class
                                                     ((and (listp parsed-type)
                                                           (second parsed-type)
                                                           (equal (first parsed-type) :class)) (second parsed-type))
                                                     ;; in class reference
                                                     ((and (listp parsed-type)
                                                           (if (listp (second parsed-type)) (second (second  parsed-type)))
                                                           (if (listp parsed-type) (or (equal (first parsed-type) :reference)
                                                                                       (equal (first parsed-type) :const-reference)))
                                                           (if (listp (second parsed-type)) (equal (first (second  parsed-type)) :class)))
                                                      (second (second  parsed-type))))))
                                        (if (listp type) `( ,@type ,sym)
                                            `( ,type ,(if name `(cxx-ptr ,sym)
                                                          sym)))))
                       nil)
      (let ((type (cffi-type arg-types)))
        (if (listp type) type
            (list type)))))

(defun parse-function (meta-ptr)
  "Retruns the function def."
  (with-foreign-slots ((name method-p class-obj func-ptr arg-types return-type) meta-ptr (:struct function-info))
    (let ((f-arg-types (if method-p
                           (left-trim-string-to arg-types #\+)
                           arg-types)))
      `(progn
         ;; don't export functions starting with '%'
         ,(if (equal #\% (char name 0))
              nil
              `(export ',(read-from-string name)))
         (,(if method-p
               'defmethod
               'defun)

           ,(read-from-string name) ,(symbols-list f-arg-types method-p class-obj)
           ;; TODO: add declare type
           ,(let ((return-val
                   `(cffi:foreign-funcall-pointer
                     ,func-ptr
                     nil
                     ,@(if method-p
                           ;; cxx-ptr defined in defclass
                           (append '(:pointer (cxx-ptr obj)) (parse-args f-arg-types))
                           (parse-args f-arg-types))
                     ,@(parse-args return-type nil))))
              ;; Wrap return class
              (let* ((parsed-type (parse-type return-type))
                     (objT (cond
                             ;; constructor
                             ((and (not method-p) class-obj)
                              (read-from-string class-obj))
                             ;; return class
                             ((and (listp parsed-type)
                                   (second parsed-type)
                                   (equal (first parsed-type) :class)) (second parsed-type))
                             ;; return class reference
                             ((and (listp parsed-type)
                                   (if (listp (second parsed-type)) (second (second  parsed-type)))
                                   (if (listp (second parsed-type)) (equal (first (second  parsed-type)) :class))
                                   (if (listp parsed-type) (or (equal (first parsed-type) :reference)
                                                               (equal (first parsed-type) :const-reference))))
                              (second (second  parsed-type))))))
                (cond
                  ;; add finalizer to class object
                  (objT
                       (let ((destructor
                               (read-from-string
                                (concatenate 'string
                                             "destruct-" (string objT)))))
                       `(let* ((ptr ,return-val)
                               (obj (handler-case
                                        (make-instance ',objT
                                                       :cxx-ptr ptr)
                                      (error (err)
                                        (,destructor ptr)
                                        (error err)))))
                          (tg:finalize obj (lambda ()
                                             (,destructor ptr)))
                          obj)))
                  ;; add finalizer to string
                  ((equal parsed-type :string)
                   (tg:finalize return-val
                                (lambda ()
                                  (cxx:destruct-string return-val)))
                   return-val)
                  ;; non-class return type
                  (t return-val)))))))))

(defun parse-super-classes (s)
  "Returns super class as symbols in a list"
  (if s (loop
           for i in (split-string-by s #\+)
           collect (read-from-string i))))

(defun parse-class-slots (slot-names slot-types)
  "Returns super class as symbols in a list"
  (loop
     ;; TODO: use solt type and define set,get
     for name in (split-string-by slot-names #\+)
     for type in (split-string-by slot-types #\+)
     collect (read-from-string name)))


(defun parse-class (meta-ptr)
  "Define class"
  (with-foreign-slots ((name super-classes slot-names slot-types constructor destructor) meta-ptr (:struct class-info))
    `(progn
       (defclass ,(read-from-string name) ,(parse-super-classes super-classes)
         ((cxx-class-ptr
           :accessor cxx-ptr
           :initarg :cxx-ptr
           :initform (required "Use Class constructor function.")))
          ;; TODO: add slots
          ;; ,@(if slot-types (parse-class-slots slot-names slot-types)))
         (:documentation "Cxx class stored in lisp"))

       (import 'cxx::cxx-ptr)
       (export 'cxx-ptr)

       (export ',(read-from-string (concatenate 'string "destruct-" name)))
       (defun ,(read-from-string (concatenate 'string "destruct-" name)) (class-ptr)
         "delete class"
         (if (not  (cffi:null-pointer-p class-ptr))
             (cffi:foreign-funcall-pointer ,destructor nil :pointer class-ptr :void)))

       ,(if (not (cffi:null-pointer-p  constructor))
            (let ((m-name (read-from-string (concatenate 'string "create-" name)))
                  (destructor (read-from-string (concatenate 'string
                                                             "destruct-" name))))
              `(progn
                 (export ',m-name)
                 (defun ,m-name ()
                   "create class with defualt constructor"
                   (let* ((ptr (cffi:foreign-funcall-pointer
                                ,constructor nil :pointer))
                          (obj (handler-case (make-instance ',(read-from-string name)
                                                            :cxx-ptr ptr)
                                 (error (err) (,destructor ptr)
                                        (error err)))))
                     (tg:finalize obj (lambda ()
                                        (,destructor ptr)))
                     obj))))))))



(defun parse-constant (meta-ptr)
  "Define constant"
  (with-foreign-slots ((name value) meta-ptr (:struct constant-info))
    `(progn
       (export ',(read-from-string name))
       (defconstant ,(read-from-string name)
         ,(read-from-string value)))))

;; inline void lisp_error(const char *error)
(defcallback lisp-error :void ((err :string))
  (format t "Caught error: ~a~%" err))

;; void send_data(MetaData *M, uint8_t n)
(defcallback reg-data :void ((meta-ptr :pointer) (type :uint8))
  (ecase type
    (0
     ;; (print "class")
     ;; (print (parse-class meta-ptr))
     (eval (parse-class meta-ptr)))

    (1
     ;; (print "constant")
     ;; (print (parse-constant meta-ptr))
     (eval (parse-constant meta-ptr)))

    (2
     ;; (print "function")
     ;; (print (parse-function meta-ptr))
     (eval (parse-function meta-ptr)))))


;; Init. clcxx
(defun init ()
  (clcxx-init (callback lisp-error) (callback reg-data)))

(defun add-package (pack-name func-name)
  "Register lisp package with pack-name
            from func-name defined in ClCxx lib"
  (declare (type string pack-name func-name))
  (let ((curr-pack (package-name *package*)))
    (unwind-protect
         (progn
           (make-package pack-name)
           (eval `(in-package ,pack-name))
           (register-package pack-name (foreign-symbol-pointer func-name)))
      (eval `(in-package ,curr-pack)))))

(defun remove-package (pack-name)
  (if (remove-c-package pack-name)
      (delete-package pack-name)))
