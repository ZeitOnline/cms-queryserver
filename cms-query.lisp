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


(defclass compiler-context ()
  ((stack    :accessor opcode-stack :initform ())
   (tables   :accessor tables-of    :initform ())
   (bindings :accessor bindings-of  :initform ()))
  (:documentation ""))

(defun make-clark-name (namespace name)
  (declare (type (namespace string))
	   (type (name string)))
  (format NIL "{~A}~A" namespace name))

(defun lookup-binding (name compiler)
  )

(defun add-table (compiler-context)
  (push (format NIL "tmp_~d" (length (tables-of compiler-context)))  (tables-of compiler-context)))

(defun make-qualified-token (table field)
  (format NIL "~A.~A" table field))

(defgeneric render-query (opcode context &optional stream)
  (:method ((opcode opcode) context &optional stream)
    (error "~&Cannot compile OP-Code ~A to SQL" (class-of opcode))))

(defmethod render-query ((sop set-union) (context compiler-context) &optional (stream *standard-output*))
 ;;; Here we _assume_ the all subtrees can be unified!
 ;;; We should probably write an assertion for that.
  (format nil "~{~A~^UNION ~}" (mapcar #'(lambda (st) (render-query st context)) (branches-of sop))))

(defmethod render-query ((sop set-intersection) (context compiler-context) &optional (stream *standard-output*))
  (with-output-to-string (s)
  (loop FOR (this other) ON (branches-of sop)
       WHEN other DO (format s "~A  INNER JOIN ON (~A = ~A) ~%" 
			     (render-query this context)
			     (make-qualified-token (table-name-of this) "uri")
			     (make-qualified-token (table-name-of other) "uri"))
       ELSE DO (format s "~A" (render-query this context)))))

(defmethod render-query ((constraint filter-constraint) (context compiler-context) &optional (stream *standard-output*))
  (format nil "(SELECT uri, namespace, name FROM facts where ~
 namespace = '~A' AND   name = '~A'  AND value = '~A') "  ; AS ~A "
	  (namespace constraint) (name constraint)
	  (value constraint) "foo";(cname constraint)
	  ))

(defmethod render-query ((constraint binding-constraint) (context compiler-context) &optional (stream *standard-output*))
  (format nil "(SELECT uri, namespace, name, value FROM facts where ~
 namespace = '~A' AND   name = '~A') " ;AS ~A "
	  (namespace constraint) (name constraint) "bar";(cname constraint)
	  ))

(defmethod render-query ((constraint range-constraint) (context compiler-context) &optional (stream *standard-output*))
  (format stream "(SELECT uri, namespace, name FROM facts where ~
 namespace = '~A' AND   name = '~A'  AND value >= ~A AND value <= ~A) " ;AS ~A "
	  (namespace constraint) (name constraint)
	  (lower-bound constraint) (upper-bound constraint) (cname constraint)))


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

;;; compile-query:
;;; takes an (s-expression formated) query and returns an AST
(defun compile-query (spec)
  ""
  (ecase (opcode spec)
    (:and (with-op-class set-intersection
	    (setf (branches-of set-intersection)
		  (loop for node in (opargs spec) collect (compile-query node)))))
    (:or (with-op-class set-union
	    (setf (branches-of set-union)
		  (loop for node in (opargs spec) collect (compile-query node)))))
    (:is (apply #'make-filter-constraint := (opargs spec)))
    (:eq (apply #'make-filter-constraint := (opargs spec)))
    (:gt (apply #'make-filter-constraint :> (opargs spec)))
    (:lt (apply #'make-filter-constraint :< (opargs spec)))
    (:bind (apply  #'make-binding-constraint (opargs spec)))
    (:between (error 'cms-query-error :explanation "OPCODE :between not yet supported!"))))

(defgeneric compile-sql (ast context)
  (:documentation "Compile a pre-comiled query into a corresponding SQL qwery."))

(defgeneric scan-opcode (opcode context))

(defmethod scan-opcode (opcode context)
  (values))

(defmethod scan-opcode ((op set-operation) (context compiler-context))
  (setf (table-name-of op) (add-table context))
  (loop for opcode in (branches-of op) do (scan-opcode opcode context)))

;;; Here we collect binding information needed to generate the result
;;; formatter as well as the SQL query
(defmethod scan-opcode ((op binding-constraint) (context compiler-context))
  (pushnew op (bindings-of context)))

(defmethod compile-sql (ast context)
  (scan-opcode ast context)
  (with-output-to-string (sql)
    ;;; Walk the AST to collect binding information
    
    ;;; Emmit SELECT stanza
    (format sql "SELECT foo FROM")

    sql))

#-DEPLOYMENT
(defun test-query (ast)
  (compile-sql ast (make-instance 'compiler-context)))

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
