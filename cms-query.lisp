o;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :cms-query)

(defparameter *parent-opcode* NIL
  "This will be dynamically bound during query
parsing to the containing opcode or NIL.")

(defclass logic-variable ()
  ((name :accessor name :initarg :name))
  (:documentation "An unnamed logic variable"))

(defclass named-logic-variable (logic-variable)
  ((binding :accessor binding :initarg :binding :initform NIL))
  (:documentation "A named logic variable."))

(defmethod print-object ((object logic-variable) stream)
  (format stream "?_"))

(defmethod print-object ((object named-logic-variable) stream)
  (format stream "?~A" (name object)))

(defmethod bind ((object logic-variable) value)
  (setf (binding object) value))

;;; Support for namespace shortcuts
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *ns-map* (make-hash-table)
  "Hash that maps from keywords to namespaces.")

  (defun register-namespace (keyword namespace)
    "Register a namespace shortcut -KEYWORD will be substituted with NAMESPACE in queries."
    (setf (gethash keyword *ns-map*) namespace))
  (export 'register-namespace))

(register-namespace :document "http://namespaces.zeit.de/CMS/document")
(register-namespace :workflow "http://namespaces.zeit.de/CMS/workflow")

(defun lookup-namespace (keyword)
  (multiple-value-bind (namespace found) (gethash keyword *ns-map*)
    (when (not found) (error 'invalid-namespace-prefix :prefix keyword))
    namespace))

(declaim (inline resolve-namespace))
(defgeneric resolve-namespace (ns))

(defmethod resolve-namespace ((ns string))
  ns)

(defmethod resolve-namespace ((ns symbol))
  (lookup-namespace ns))

;;; Here follow classes implementing the query opcodes

(defparameter *opcodes* NIL
  "List of all query opcodes known to the system.")

;;; Not used for now - we want to provide this to be able to generate
;;; a list of all known opcodes to implement better error reporting.
(defmacro register-opcode (name supers  &optional (slot-spec (list)) (options (list)))
  `(defclass ,name ,supers ,slot-spec ,options))

(defclass opcode () ())

(defclass set-operation (opcode)
  ((branches   :accessor branches-of))
  (:documentation ""))

(defclass set-union (set-operation)
  ()
  (:documentation ""))

(defclass set-intersection (set-operation)
  ()
  (:documentation ""))

(defclass constraint (opcode)
  ((table-name :accessor table-name-of) 
   (cname      :initarg  :cname        :accessor cname-of)
   (name       :initarg  :name         :accessor name-of)
   (namespace  :initarg  :namespace    :accessor namespace-of))
  (:documentation ""))

(defmethod initialize-instance :after ((self constraint) &key &allow-other-keys)
  (setf (cname-of self) (make-clark-name (namespace-of self) (name-of self))))

(defclass filter-constraint (constraint)
  ((value :initarg :value :accessor value-of))
  (:documentation ""))

(defclass relational-constraint (filter-constraint)
  ((operator :initarg :operator :accessor operator-of)))

(defclass eq-constraint (relational-constraint)
  ()
  (:default-initargs :operator 'is))

(defclass gt-constraint (relational-constraint)
  ()
  (:default-initargs :operator 'gt))

(defclass lt-constraint (relational-constraint)
  ()
  (:default-initargs :operator 'lt))

;;; functional interface: this makes query compilation slightly more
;;; readable (we save destructuring)
(defun make-filter-constraint (rel-op namespace name value)
  (make-instance
   (ecase rel-op
     (:= 'eq-constraint)
     (:< 'lt-constraint)
     (:> 'gt-constraint))
   :namespace (resolve-namespace namespace)
   :name name
   :value value))


(defclass binding-constraint (constraint)
  ((parent  :initarg :parent  :initform *parent-opcode*)
   (varname :initarg :varname :accessor varname))
  (:documentation ""))

;;; see notes above
(defun make-binding-constraint (namespace name value)
  (make-instance 'binding-constraint
		 :namespace (resolve-namespace namespace)
		 :name name
		 :varname (symbol-name  value)))

(defclass range-constraint (constraint)
  ((from :initarg :from :accessor lower-bound)
   (to   :initarg :to   :accessor upper-bound))
  (:documentation "Match a value in the range [FROM TO]"))

(defun make-range-constraint (namespace name from to)
  (make-instance 'range-constraint 
		 :namespace namespace
		 :name name
		 :from from
		 :to to))

(defclass member-constraint (constraint)
  ((values :initarg :values :accessor values-of))
  (:documentation ""))

(defun make-member-constraint (namespace name &rest values)
  (warn "Selecting in ~A" values)
  (make-instance 'member-constraint 
                 :namespace namespace 
                 :name name 
                 :values values))


;;; The query compiler  
(defclass compiler-context ()
  ((stack           :accessor opcode-stack      :initform ())
   (tables          :accessor tables-of         :initform ())
   (bindings        :accessor bindings-of       :initform ())
   (binders         :accessor binders-of        :initform ())
   (filters         :accessor filters-of        :initform ())
   (predicate-names :initform (make-hash-table)))
  (:documentation ""))

;;; Each binding needs a unique name to be used as a table name during
;;; sql compilation.
(defun add-predicate-name (context predicate)
  (with-slots (predicate-names) context
    (let ((name (gethash predicate predicate-names)))
      (if name name
          (setf (gethash predicate predicate-names) (make-symbol  (add-table context)))))))

(defun get-predicate-name (context predicate)
  (with-slots (predicate-names) context
    (or (gethash predicate predicate-names)
        (error "Severe compiler error ~A" predicate))))

(defun make-clark-name (namespace name)
  (declare (type (namespace string))
	   (type (name string)))
  (format NIL "{~A}~A" namespace name))

;; STALE?
;; (defun lookup-binding (name compiler)
;;   )

;;; Add a table the compiler context and return the newly created table
;;; name.
(defun add-table (compiler-context)
  (let ((name (format NIL "tmp_~d" (length (tables-of compiler-context)))))
    (push name (tables-of compiler-context))
    name))

(defun make-qualified-token (table field)
  (format NIL "~A.~A" table field))

(defun opcode (thing)
  (let ((code (first thing)))
    (unless (keywordp code)
      (error "~A is not a valid OPCODE" code))
    code))

(defun opargs (thing)
  (rest thing))

(defmacro with-op-class (cname &body body)
  `(let* ((,cname (make-instance ',cname))
          ( *parent-opcode* ,cname))
     ,@body
     ,cname))

#|

 SELECT <p-list> FROM <table-aliases> <table-chain>

where <p-list>  is a list of (uri <qualified field names>+)
and <table-chain> a sequence of  (:select <preds> <binders>)
joined by :inner-join <left> :on (:= (:dot (table-name <left>) uri) (:dot (table-name <right>) uri)) 
|#

;;; collect-p-list is a helper function to generate a projection list,
;;; i.e. a list of (qualified) fields to be returned by a sql select
;;; query.

(defgeneric collect-p-list (opcode context &key &allow-other-keys)
  (:method ( opcode context &key &allow-other-keys) NIL))

(defmethod collect-p-list ((opcode binding-constraint) context &key parent &allow-other-keys)
  `(,(make-symbol (format NIL "~a.value" (get-predicate-name context opcode)))))

(defmethod collect-p-list ((opcode set-intersection) context &key &allow-other-keys)
  (loop for oc in (branches-of opcode)
     when (collect-p-list oc context :parent opcode) append it))

;;; compile-query:
;;; takes an (s-expression formated) query and returns an AST
(defun compile-query (query)
  ""
  ;;; Some simple error checking ...
  ;;; Technically speaking keywordp is wrong but it eases development
  ;;; for now.
  (assert (and (listp query) (keywordp (car query))) (query)
          'cms-malformed-query 
          :explanation "Your query doesn't seem to be a valid query")

  (ecase (opcode query)
    (:and (with-op-class set-intersection
	    (setf (branches-of set-intersection)
		  (loop for node in (opargs query) collect (compile-query node)))))
    (:or (with-op-class set-union
	    (setf (branches-of set-union)
		  (loop for node in (opargs query) collect (compile-query node)))))
    (:is (apply #'make-filter-constraint := (opargs query)))
    (:eq (apply #'make-filter-constraint := (opargs query)))
    (:gt (apply #'make-filter-constraint :> (opargs query)))
    (:lt (apply #'make-filter-constraint :< (opargs query)))
    (:bind (apply  #'make-binding-constraint (opargs query)))
    (:member-p (apply  #'make-member-constraint (opargs query)))
    (:between (error 'cms-query-error :explanation "OPCODE :between not yet supported!"))))

(defgeneric scan-opcode (opcode context))

(defmethod scan-opcode (opcode context)
  (values))

(defmethod scan-opcode ((op set-operation) (context compiler-context))
  (setf (table-name-of op) (add-table context))
  (loop for opcode in (branches-of op) do (scan-opcode opcode context)))

(defmethod scan-opcode ((op constraint) (context compiler-context))
  (add-predicate-name context op))

;;; Here we collect binding information needed to generate the result
;;; formatter as well as the SQL query
(defmethod scan-opcode ((op binding-constraint) (context compiler-context))
  (pushnew op (bindings-of context))
  (call-next-method))

(defun build-query (qspec)
  (let ((filters NIL)
	(binders NIL)
	(constraints NIL)
	(result-vars NIL)
	(ccount 0))

    (unless (is-opcode-p qspec)
      (error "Wrong query format! ~S" qspec))
    (warn "QSPEC -> ~S"(rest qspec))
    (dolist (c (rest qspec))

      (ecase (opcode c)
	(:bind (destructuring-bind (namespace name value) (opargs c)
                 (push (make-instance 'binding-constraint
				    :cname (make-tmp-table-name ccount)
				    :namespace (resolve-namespace namespace)
				    :name name
				    :varname (symbol-name  value)) binders)))
	(:is (destructuring-bind (namespace name value) (opargs c)
               (push (make-instance 'filter-constraint
                                    :cname (make-tmp-table-name ccount)
                                    :namespace (resolve-namespace namespace)
                                    :name name
				      :value value) filters)))
	(:in (error "Query opcode :in not yet supported!")))
      (incf ccount))
    (setf constraints (append filters binders))
    (loop for c in constraints
       when (eq (type-of c) 'binding-constraint)
       do (push
           (format nil "~A AS ~A"
                   (make-qualified-token (cname c) 'value) (varname c))
           result-vars))
    (when (= (length result-vars) 0)
      (error "No variables to return?"))
    (with-output-to-string (query)
      (format query "SELECT tmp_0.uri, ~{~a~^, ~} FROM " result-vars)
      (dotimes (i (length constraints))
	(if (= i 0)
	    (render-query (nth i constraints) query)
	    (progn
	      (format query " INNER JOIN ")
	      (render-query (nth i constraints) query)
	      (format query " ON (~A=~A) "
		      (make-qualified-token (cname (nth (1- i) constraints)) 'uri)
		      (make-qualified-token (cname (nth i constraints)) 'uri))))))))


(defun sql-escape-field (thing)
  `(:raw ,(format NIL "\"~a\"" thing)))

(defun generate-plist (bindings uri-source)
  (loop for (table alias) in bindings 
     collect `(:as (:dot (:raw ,table) value) ,(sql-escape-field alias)) 
     into plist
     finally (return plist)))

(defun generate-constraint-list (constraints)
  (loop with clist = (list) 
     for (this . that) on (reverse constraints) 
     do (warn "Processing ~A ~A" this that)
     when that ; not the first constraint
     do (progn            
          (push `(:= (:dot ,(sql-escape-field (table-name-of (car that))) uri ) 
                     (:dot ,(sql-escape-field (table-name-of this)) uri)) clist)
          (push :on  clist))
     do (push (generate-sql this) clist)
     when that 
     do (push :inner-join clist)
     finally (return clist)))

(defgeneric generate-sql (node))

(defmethod generate-sql (node)
  (error "Don't know how to generate SQL for ~A" node))

(defmethod generate-sql ((node binding-constraint))
  `(:as (:select uri value
                 :from facts 
                 :where (:and (:= namespace ,(namespace-of node))
                              (:= name ,(name-of node)))) 
        (:raw ,(table-name-of node))))

(defmethod generate-sql ((node filter-constraint))
  `(:as (:select uri
                 :from facts 
                 :where (:and (:= namespace ,(namespace-of node))
                              (:= name ,(name-of node))
                              (:= value ,(value-of node))))
        (:raw ,(table-name-of node))))

(defun compile-sql (query)
  "Compile a query to a SQL query."
  (let ((compiler-context (make-instance 'compiler-context)))
    (labels ((scan (node)
               (let ((table-name (add-table compiler-context)))
                 (when (typep node 'binding-constraint)
                   (push (list  table-name (cname-of node)) (bindings-of compiler-context)))
                 (when (typep node 'constraint)
                   (setf (table-name-of node) table-name)
                   (push node (binders-of compiler-context)))
                 ;; process branches
                 (when (typep node 'set-operation)
                   (loop for node in (branches-of node)
                      do (scan node))))))

      (scan query)
      ;; debugging
      `(:select  
        (:dot ,(sql-escape-field (table-name-of (first (binders-of compiler-context)))) uri) 
        ,@(generate-plist (bindings-of compiler-context) nil) 
        :from ,@(generate-constraint-list (binders-of compiler-context)))
      )))


;;; Running queries from the REPL
(defmacro run-query (query)
  "Run query with SPECS and return the result list."
  `(clsql:query (sql-compile (compile-sql (compile-query ',query)))))

(defmacro show-query (query)
  "Run query with SPECS and return the result list."
  `(sql-compile (compile-sql (compile-query ',query))))

(defun find-resources (specs)
  (clsql:query (build-query specs)))

;;; Pretty-printing query
(defun indent (depth) 
  "Helper function for pretty-printing."
  (make-string depth :initial-element #\tab))

(defparameter *pp-operator-map*
  '((is . "=")
    (gt . ">")
    (lt . "<")
    (in . "is one of")))

(defun pp-operator (op)
  (cdr (or (assoc op *pp-operator-map*)
            (cons op op))))

(defgeneric pprint-query (q &optional stream level))

 (defmethod pprint-query (query &optional (stream T)  (level 0))
  (format T "~%~A ~A" (indent  level) (class-name (class-of query))))

(defmethod pprint-query ((query relational-constraint) &optional (stream T)  (level 0))
  (format T "~%~A ~A (resources where ~A ~A ~A" 
          (indent  level) (class-name (class-of query))
          (cname-of query) (pp-operator (operator-of query)) (value-of query)))

(defmethod pprint-query :after (query &optional (stream T)  (level 0))
  (format stream "~%"))

(defmethod pprint-query :after ((query set-operation) &optional (stream T) (level 0))
  (loop for query in (branches-of query) do (pprint-query query stream (1+ level))))

(defun walk-query (query)
  )