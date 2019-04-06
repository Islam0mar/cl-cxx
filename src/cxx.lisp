
(in-package :cxx)

(defun symbols-list (arg-types)
  "Return a list of symbols '(V0 V1 V2 V3 ...) 
   representing thenumber of args"
  (if arg-types (loop for i in (split-string-by arg-types #\+)
                   for j from 0
                   collect (intern (concatenate 'string "V" (write-to-string j))))))

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
                              (:pointer :pointer)
                              (:reference :pointer)
                              (:class :pointer)
                              (:struct (second parsed-type)))
        parsed-type)))


(defun parse-args (arg-types &optional (input-type-p t))
  "return argument types (with variables if they are inputs) in a proper form"
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
    (if method-p
        `()
        `(defun ,(read-from-string name) ,(symbols-list arg-types)
           ;TODO: add declare type
           ,(if arg-types `(cffi:foreign-funcall-pointer
                            ,thunc-ptr
                            nil
                            ,@(parse-args arg-types)
                            ,@(parse-args return-type nil))
                `(cffi:foreign-funcall-pointer 
                  ,thunc-ptr
                  nil
                  ,@(parse-args return-type nil)))))))

(defun parse-class (meta-ptr)
  "Define class"
  (with-foreign-slots ((name super-classes slot-names slot-types constructor destructor) meta-ptr (:struct class-info))
    ;TODO:
    ))

(defun parse-constant (meta-ptr)
  "Define constant"
  (with-foreign-slots ((name value) meta-ptr (:struct constant-info))
    ;TODO:
    ))

;; inline void lisp_error(const char *error)
(defcallback lisp-error :void ((err :string))
  (format t "Caught error: ~a~%" err))

;; void send_data(MetaData *M, uint8_t n)
(defcallback reg-data :void ((meta-ptr :pointer) (type :uint8))
  (ecase type
    (0 (print "class")
       (with-foreign-slots ((name super-classes slot-names slot-types constructor destructor) meta-ptr (:struct class-info))
         (format t "name:~A super-classes:~A slot-names:~A slot-types:~A constructor:~A destructor:~A~%"
                 name super-classes slot-names slot-types  constructor destructor)))
    (1 (print "constant")
       (with-foreign-slots ((name value) meta-ptr (:struct constant-info))
         (format t "name:~A value:~A~%"
                 name value)))
    (2 (print "function")
       (eval (parse-function meta-ptr))
       (print "~%")
       (with-foreign-slots ((name method-p class-obj thunc-ptr func-ptr arg-types return-type) meta-ptr (:struct function-info))
         (format t "name:~A method-p:~A class-obj:~A thunc-ptr:~A func-ptr:~A arg-types:~A return-type:~A~%"
                 name method-p class-obj thunc-ptr func-ptr arg-types return-type)))))

    
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
  (let ((curr-pack *package*))
    (make-package pack-name)
    (use-package pack-name)
    (register-package pack-name (foreign-symbol-pointer func-name))
    (use-package curr-pack)))

(defun remove-package (pack-name)
  (if (remove-c-package pack-name)
      (delete-package pack-name)))


