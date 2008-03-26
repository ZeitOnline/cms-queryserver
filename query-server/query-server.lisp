;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     query-server.lisp
;;;; Purpose:  Implements the main server
;;;; Author:   Ralf Mattes 
;;;; Created:  2007-08-22
;;;; Version:  $Id$
;;;; Copyright 2007 Ralf Mattes
;;;;           Full licence at end of file.
;;;; Commentary:
;;;;
;;;; 
;;;;
;;;; *************************************************************************


(in-package :cms-query)

(defvar *server*  NIL)
(defvar *port*    9876)
(defvar *verbose* T)
(defvar *address* "localhost")
(defvar *last-query* NIL)
(defvar *database-pool* NIL "Default database connection pool")


;; (defun  handle-search-request ()
;;   "Returns a WebDAV compliant XML response"
;;   (setf (tbnl:header-out "X-QENGINE") "CMS Query Server v0.1alpha")
;;   (setf (tbnl:content-type) "text/xml; charset=UTF-8")
;; ;FIXME: (setf (tbnl:reply-external-format) "UTF-8")
;;   (let* ((*read-eval* NIL)
;;          (request-query (tbnl:raw-post-data)))
;;     (setf *last-query* request-query)

;;     (let* ((context (make-instance 'compiler-context))
;;            (query   (compile-query (read-from-string  request-query)))
;;            (sql     (build-query (scan-opcode query context))))
;;       ;; Insert WebDAV response
;;       (format NIL "~&<!--~&~A~%-->~%~S~% " sql  
;;               (dav:serialize-xmls-node (apply #'dav:dav-node (list "multistatus" "")))))))

(defgeneric handle-interactive-request ()
  (:documentation "Presents a HTML input form for interactive queries.
Returns a tabualted list of results."))

(defmethod handle-interactive-request :before ()
  (warn "About to handle an interactive request/response"))

(defmethod handle-interactive-request ()
  "Returns a HTML formated response"
  (let ((query (or (post-parameter "query") ""))
        (has-query (post-parameter "query")))
    (warn "Got query ~A" (read-from-string  query))
    (with-output-to-string (s) 
    (cl-who:with-html-output (*standard-output* s :indent t)
      (:div :class "formcontainer"
            (:form :method :post :enctype "multipart/form-data"  ;:action ""
                   (:div :class "formpostfield"
                         (:p "Query:" 
                             (:br) 
                             (:textarea :name "query" :rows 10 :cols 30  (cl-who:str query)
                                        )))
                   (:div :class "formsubmitfield"
                         (:input :type :reset))
                   (:div :class "formsubmitfield"
                         (:input :type :submit))
                   (:dic :class "pp-query" 
                         (:p  
                          (cl-who:fmt "~S"  
                           (if has-query 
                               (cms-query::find-resources (read-from-string query)) 
                               "No query found"))))))))))

(defgeneric cms-query-server-dispatch (request)
  (:documentation "Handle a query request. 
This needs to be a generic function since we want to us
method combinations."))

;;; We need this arround-method to set up the resources needed to handle
;;; the request
(defmethod cms-query-server-dispatch :around (request)

  (prog1  (call-next-method)
    
    ))

(defmethod cms-query-server-dispatch (request)
  "Only handle SEARCH method requests"
  (ecase (tbnl:request-method request)
    (:search 'handle-search-request)
    (:post   'mockup-handler)
    (:get    'mockup-handler)))

;;; Main entry point: the following functions start/stop the server

(defun start (&key (address *address*) (port *port*) (verbose-p *verbose*))
  (prog1 
      ;; FIXME: we need to set up a database connection pool somewhere
      ;; arround here
      (setf *server*  (tbnl:start-server 
                       :address address :port port 
                       :dispatch-table (list #'cms-query-server-dispatch)
                       :name "CMS QUERY SERVER"
                       ))
    (when verbose-p 
      (format *trace-output* "~&Started query server on ~A port ~A~%" address port))))

;;; FIXME: make shure the server gets destroyed after shutdown
(defun stop (&optional (server *server*))
  "Stop the CMS Query Server."
  (if server
      (progn 
        ;; FIXME: destroy the database connection pool here
        (tbnl:stop-server server)
        (setf *server* nil))
      (warn "Server not running!")))

(defun exit ()
  "Stop the server and exit the application."
  (stop)
  (sb-ext::quit :recklessly-p T :unix-status 0))

;;; Some useful helper code:
;;; generating just the right xmls nodes

(declaim (inline clark-to-ns-name))
(defun clark-to-ns-name (clark-name)
  (cl-ppcre:register-groups-bind 
   (namespace local-name)
   ("{([^}]+)}(.+)" clark-name)
    (list namespace local-name)))

(declaim (inline make-prop-node))
(defun make-prop-node (clark-name value)
  "Generate a XMLS node for the property with 
   the given Clark name and value"
  (cl-ppcre:register-groups-bind 
   (namespace local-name)
   ("{([^}]+)}(.+)" clark-name)
    (dav:dav-node "prop" 
   (dav::make-xmls-node :local-name local-name
                   :namespace-uri namespace
                   :children (list value)))))

;;; -->
(declaim (inline make-resource-node))
(defun make-resource-node (href status)
  "Generate a XMLS node for the resouce with URI href and status code status"
  (cl-webdav:dav-node "response"
                      (list )))

;;; FIXME: get rid of package names

(defun construct-xml-node (binding)
  (let ((name (cms-query::name binding))
        (namespace (cms-query::namespace binding))
        (value (cms-query::cname binding)))
    `((,name ,namespace) NIL  ,value)))

(defun compile-webdav-formatter (query context)
  (let ((uri (gensym))
        (prop-emitter                   ; Not used for now
         (loop for binding in (cms-query::bindings-of context)
            ;; build a XMLS node structure
            collect
              `(("prop" . "DAV:") NIL
                ((,(cms-query::name binding) . ,(cms-query::namespace binding)) NIL
                 ,(cms-query::cname binding)))))
    
        ;; Arguments to the emitted Lambda form 
        (fun-args
         (loop for binding in
              (cms-query::bindings-of context)
            collect
              (intern
               (cms-query::cname binding)))))

    ;; This is the code we emmit
    (compile NIL `(lambda (,uri ,@fun-args)
                    `(("response"  . "DAV:") NIL
                      (("href"     . "DAV:") NIL ,,uri)
                      (("propstat" . "DAV:")
                      NIL
;                       ,',@',(loop 
                       ,,@(loop 
                              for binding in (cms-query::bindings-of context)
                              for namespace = (cms-query::namespace binding)
                              for name  = (cms-query::name binding)
                              for param = (intern (cms-query::cname binding))
                              ;; for param = (cms-query::cname binding)
                              ;; build a XMLS node structure
                              collect
                                 `(("prop" . "DAV:") NIL
                                   ((,name . ,namespace) NIL
                                    ,`,param ; <-- Wrong expansion, we need a komma more here!
                                "Yers plain ol' dummy, sincerly") )))
                      (("status" .  "DAV:") NIL "HTTP/1.1 200 OK")))) 
    ))

(defun handle-search-request ()
  "Handler that queries the matadata store and returns
a WebDAV propget response."
  (let (query sql-query)
    
    (setf
     (header-out "Server") "CMS-Query-Server"
     (header-out "X-Handled-By") "handle-search-request"
     (content-type) "text/xml; charset=utf-8"
     (return-code) +http-multi-status+)

    ;; FIXME: add error handling
    (let* ((*read-eval* NIL))
      (setf query (read (tbnl:raw-post-data  :want-stream t))))
    
    (setf sql-query (sql-compile (compile-sql (compile-query query))))
    (warn "SQL: ~A" sql-query)
    (setf (reply-external-format)  (flex:make-external-format :utf-8 :eol-style :lf))
    
    (with-output-to-string (s)
      (multiple-value-bind (tuples fields) (clsql:query sql-query :flatp t)
        (format s "<?xml version='1.0' encoding='utf-8' ?>
<D:multistatus xmlns:D='DAV:'>~%")
        (loop for tuple in tuples
           do (progn  (format s "~&<D:response>~&<D:href>~A</D:href>~%<D:propstat>~%<D:prop>" (first tuple))
                      (loop for value in (rest tuple) and
                         fname in (rest fields)
                         do (destructuring-bind (ns name) (clark-to-ns-name fname)
                              (format s "~&<~A xmlns='~a'>~a</~A>" name ns (hunchentoot:escape-for-html value) name)))
                      (format s "~&</D:prop>~%<D:status>HTTP/1.1 200 OK</D:status>~%</D:propstat>~%</D:response>")))
        (format s "</D:multistatus>")))))

  


#-DEPLOYMENT 
(defun mockup-handler ()
  "Mokup handler to emit fake query responses"

  (setf
   (header-out "Server") "CMS-Query-Server"
   (header-out "X-Handled-By") "mokup-handler"
   (content-type) "text/xml; charset=utf-8"
   (return-code) +http-multi-status+)

  ;;; Read the query from the POST data ...
  ;;; FIXME: this (setf (tbnl:reply-external-format) "UTF-8") doesn't
  ;;; work.

  (let* ((*read-eval* NIL)
         (query    (cms-query::compile-query  (read (tbnl:raw-post-data  :want-stream t))))
         (context  (make-instance 'cms-query::compiler-context))
         sql-query dummy-tuple formatter
         ;; (response (tbnl:send-headers))
         )
    
    ;; Setup the compiler context
    (cms-query::scan-opcode query context)
    ;; Now we should have collected all bindings as well as all table
    ;; names, hence we can compile a result formatter
    (setf formatter (compile-webdav-formatter query context))
    
    ;; Create a dummy value list
    (setf dummy-tuple (loop for val from 1 to (length (cms-query::bindings-of context))
                         collect (format NIL "value-~a" val)))
    
    (loop for resource from 1 to 24 ; or, (random 256) for that matter ...
       collect (apply formatter (format NIL "/cms/work/2007/resource-~A" resource) dummy-tuple)
       into result 
       finally (return  (sb-ext:octets-to-string 
                         (cl-webdav:serialize-xmls-node (apply #'cl-webdav:dav-node "multistatus" result))
                         :external-format :utf-8))
         )))


;;;; *************************************************************************
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
