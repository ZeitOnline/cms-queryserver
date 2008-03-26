(defpackage #:cms-query 
  (:use :cl :s-sql :hunchentoot :cl-who :lisa-lisp)
  (:export 
   #:*ns-map*
   #:find-resources
   #:register-namespace
   #:run-query))

(defpackage #:cms-query-user 
  (:use :cl 
        :cms-query)
  (:nicknames :cms))
