(in-package :cms-query)

(defparameter *parent-opcode* NIL)

(define-condition cms-query-error ()
  ((explanation :initarg :explanation :accessor explanation))
  (:report (lambda (c s)
	     (format s "~A" (explanation c)))))

;;; FIXME: continue here ....
(define-condition cms-malformed-query (cms-query-error)
  ((opcode :initarg opcode)))

(define-condition cms-unsupported-query (cms-query-error)
  ())

(defclass logic-variable ()
  ((name :accessor name :initarg :name))
  (:documentation "An unnamed logic variable"))

(defclass named-logic-variable (logic-variable)
  ((binding :accessor binding :initarg :binding :initform NIL))
  (:documentation "A named logic variable"))

(defmethod print-object ((object logic-variable) stream)
  (format stream "?_"))

(defmethod print-object ((object named-logic-variable) stream)
  (format stream "?~A" (name object)))

(defmethod bind ((object logic-variable) value)
  (setf (binding object) value))

(defun process-token (token)
  )

(defun parse-qspec (qspec)
  (loop FOR token IN qspec
       WHEN (= 0 (position #\? ))
       DO (format T "~&~A" token)))

(defun find-triples (matchspec)
  (destructuring-bind (s p o) matchspec
    (apply #'select '(uri :from facts :limit 20 :offset 40))
      (format t "~&Subject: ~A~%Predicate: ~A~&Object: ~A" s p o)))


(defparameter *ns-map* (make-hash-table)
  "Hash that maps from keywords to namespaces.")

(define-condition invalid-namespace-prefix ()
  ((prefix :initarg :prefix :reader namespace-prefix))
  (:report (lambda (c s)
             (format s "No matching namespace found for prefix '~a'" (prin1-to-string
								 (namespace-prefix c))))))

;;; Support for namespace shortcuts
(eval-when (:load-toplevel :execute)
  (defun register-namespace (keyword namespace)
    "Register a namespace shortcut -KEYWORD will be substituted with NAMESPACE in queries."
    (setf (gethash keyword *ns-map*) namespace))
  (export 'register-namespace)
  (register-namespace :document "http://namespaces.zeit.de/CMS/document")
  (register-namespace :workflow "http://namespaces.zeit.de/CMS/workflow"))

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

(defclass opcode () ())

(defclass set-operation (opcode)
  ((table-name :accessor table-name-of) 
   (branches   :accessor branches-of))
  (:documentation ""))

(defclass set-union (set-operation)
  ()
  (:documentation ""))

(defclass set-intersection (set-operation)
()
(:documentation ""))

(defclass constraint (opcode)
  ((cname     :initarg :cname :accessor cname)
   (name      :initarg :name :accessor name)
   (namespace :initarg :namespace :accessor namespace))
  (:documentation ""))

(defmethod initialize-instance :after ((self constraint) &key &allow-other-keys)
  (setf (cname self) (make-clark-name (namespace self) (name self))))

(defclass filter-constraint (constraint)
  ((value  :initarg :value     :accessor value))
  (:documentation ""))

(defclass relational-constraint (filter-constraint)
  ((operator :initarg :operator :accessor operator)))

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
  ((parent    :initarg :parent :initform *parent-opcode*)
   (varname   :initarg :varname   :accessor varname))
  (:documentation ""))

;;; see notes above
(defun make-binding-constraint (namespace name value)
  (make-instance 'binding-constraint
		 :namespace (resolve-namespace namespace)
		 :name name
		 :varname (symbol-name  value)))


(defclass range-constraint (constraint)
  ((from :initarg :from :accessor lower-bound)
   (to   :initarg :to :accessor upper-bound))
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


(defmethod initialize-instance :after ((self member-constraint) &key &allow-other-keys)
  (with-slots (namespace name values) self
    (warn "NS ~a~&NAME ~a~&VALUES ~A" namespace name values)))
 
(defclass compiler-context ()
  ((stack    :accessor opcode-stack :initform ())
   (tables   :accessor tables-of    :initform ())
   (bindings :accessor bindings-of  :initform ())
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

(defun lookup-binding (name compiler)
  )

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
          (make-condition 'cms-malformed-query 
                          :explanation "Your query doesn't seem to be a valid query"))

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
		      (make-qualified-token (cname (nth i constraints)) 'uri)))
	))
      )))


(defmacro run-query (query)
  "Run query with SPECS and return the result list."
  `(clsql:query (build-query ',query)))

(defun find-resources (specs)
  (clsql:query (build-query specs)))
