
(in-package :cxx)

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
  (declare (type string type))
  (if (equal (subseq type 0 1) "(")
      ;; compound type
      (cond ((if (>= (length type) 7) (equal (subseq type 0 7) "(:array") nil)
             (mapcar #'parse-type (compound-type-list type t)))
            ((if (>= (length type) 9) (equal (subseq type 0 9) "(:pointer") nil)
             (mapcar #'parse-type (compound-type-list type)))
            ((if (>= (length type) 11) (equal (subseq type 0 11) "(:reference") nil)
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
                              (:class :pointer)
                              (:struct (second parsed-type)))
        parsed-type)))


(defun parse-args (arg-types &optional (input-type-p t))
  "return argument types (with variables if they are inputs) in a proper list"
  (if input-type-p (loop
                      for i in (split-string-by arg-types #\+)
                      for sym in (symbols-list arg-types)
                      as type = (cffi-type i) then (cffi-type i)
                      append (if (listp type) `( ,@type ,sym)
                                 `( ,type ,sym)))
      (let ((type (cffi-type arg-types)))
        (if (listp type) type
            (list type)))))

(defun parse-function (meta-ptr)
  "Retruns the function def."
  (with-foreign-slots ((name method-p class-obj thunc-ptr func-ptr arg-types return-type) meta-ptr (:struct function-info))
    (if method-p (setf arg-types (remove-string-after-to arg-types #\+)))
    `(progn
       (export ',(read-from-string name))
       (,(if method-p
             'defmethod
             'defun)
         
         ;; (defmethod balance ((account bank-account))
         ;;   (slot-value account 'balance))

         ,(read-from-string name) ,(symbols-list arg-types method-p class-obj)
         ;; TODO: add declare type
         ,(let ((body
                 (if arg-types `(cffi:foreign-funcall-pointer
                                 ,thunc-ptr
                                 nil
                                 :pointer ,func-ptr
                                 ,@(if method-p
                                       ;; cxx-ptr defined in defclass
                                       (append '(:pointer (cxx-ptr obj)) (parse-args arg-types))
                                       (parse-args arg-types))
                                 ,@(parse-args return-type nil))
                     `(cffi:foreign-funcall-pointer 
                       ,thunc-ptr
                       nil
                       :pointer ,func-ptr
                       ,@(parse-args return-type nil)))))
            (if (and (not method-p) class-obj)
                `(make-instance ,(read-from-string class-obj)
                                :cxx-ptr ,body)
                body))))))

(defun parse-super-classes (s)
  "Returns super class as symbols in a list"
  (if s (loop
           for i in (split-string-by s #\+)
           collect (read-from-string i))))

(defun parse-class-slots (slot-names slot-types)
  "Returns super class as symbols in a list"
  (loop
     ;; TODO: use solt type and refine set,get
     for name in (split-string-by slot-names #\+)
     for type in (split-string-by slot-types #\+)
     collect (read-from-string name)))


(defun parse-class (meta-ptr)
  "Define class"
  (with-foreign-slots ((name super-classes slot-names slot-types constructor destructor) meta-ptr (:struct class-info))
    ;; TODO:
    `(progn
       ;; (export ,(read-from-string name))
       (defclass ,(read-from-string name) ,(parse-super-classes super-classes)
         ((cxx-class-ptr
           :acessor :cxx-ptr
           :initform nil)
          ,@(if slot-types (parse-class-slots slot-names slot-types)))
         (:documentation "Cxx class stored in lisp"))

       ,(if (not (cffi:null-pointer-p  constructor))
            (let ((m-name (read-from-string (concatenate 'string "create-" name))))
              `(progn
                 (export ',m-name)
                 (defun ,m-name ()
                   "create class with defualt constructor"
                   (make-instance ,(read-from-string name) :cxx-ptr
                                  (cffi:foreign-funcall-pointer
                                   ,constructor :pointer))))))
       
       (export 'destruct)
       (defmethod destruct (obj ,(read-from-string  name))
         "delete class" 
         (cffi:foreign-funcall-pointer ,destructor :pointer (cxx-ptr obj) :void)))))



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
    (0 (print "class")
       (eval (parse-class meta-ptr)))
    (1 (print "constant")
       (eval (parse-constant meta-ptr)))
    (2 (print "function")
       (eval (parse-function meta-ptr)))))


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

;; Init. clcxx
(defun init ()
  (clcxx-init (callback lisp-error) (callback reg-data)))

(defun add-package (pack-name func-name)
  "Register lisp package with pack-name 
            from func-name defined in CXX lib"
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


