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

(defun  handle-search-request ()
  "Returns a WebDAV compliant XML response"
  (setf (tbnl:header-out "X-QENGINE") "CMS Query Server v0.1alpha")
  (setf (tbnl:content-type) "text/xml; charset=UTF-8")
;FIXME: (setf (tbnl:reply-external-format) "UTF-8")
  (let* ((*read-eval* NIL)
         (request-query (read (tbnl:raw-post-data  :want-stream t))))
    (setf *last-query* request-query)
    (format NIL "~&<!--~&~S~%-->~%~S~%" request-query (cms-query::find-resources request-query))
;; Insert WebDAV response
    (serialize-xmls-node (apply #'dav-node "multistatus" ""))))

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
    (:search 'mockup-handler)
    (:post   'mockup-handler)
    (:get    'mockup-handler)))

;;; Main entry point: the following functions start/stop the server

(defun start (&key (address *address*) (port *port*) (verbose-p *verbose*))
  (prog1 
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



#| FILTER
 (:select uri 
         :from (:as facts <table>) 
         :where (:and (:= namespace <ns>) 
                      (:= name <name>)
                      (:= value <value>)))
|#

#| BINDER
 (:select uri value
         :from (:as facts <table>) 
         :where (:and (:= namespace <ns>) 
                      (:= name <name>)
                      (:= value <value>)))
|#

(defgeneric compile-sql (opcode context)
  (:documentation "Compile a pre-comiled query into a corresponding SQL query."))

(defmethod compile-sql ((opcode binding-constraint) context)
  (with-slots (namespace name) opcode
    `(:select uri value
              :from (:as facts <table>) 
              :where (:and (:= namespace ,namespace) 
                           (:= name ,name)))))

(defmethod compile-sql ((opcode filter-constraint) context)
  (with-slots (namespace name value) opcode
    `(:select uri 
              :from (:as facts <table>) 
              :where (:and (:= namespace ,namespace) 
                           (:= name ,name)
                           (:= value ,value)))))

;;; join the sub-statements  
(defmethod compile-sql ((opcode set-intersection) context)
  (let ((p-list (collect-p-list opcode)))
      `(:select ,@p-list :from )))

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

#-DEPLOYMENT
(defparameter +mock-props+
  '(("{http://namespaces.zeit.de/CMS/document}author" "Peter Buhr")
    ("{http://namespaces.zeit.de/CMS/document}year" "2007")
    ("{http://namespaces.zeit.de/CMS/document}volume" "23")
    ("{http://namespaces.zeit.de/CMS/workflow}status" "OK")))

#-DEPLOYMENT
(defparameter +mock-resources+
  '("/work/2006/21/foo" "/work/2007/12/bar"
    "/cms/work/2006/45/01-Bundeswehr"
"/cms/work/2006/45/01-Parteien"
"/cms/work/2006/45/50-J-hrige-Donig"
"/cms/work/2006/45/515-Artgenossen-45"
"/cms/work/2006/45/515-Donnerstalk-45"
"/cms/work/2006/45/515-Koalitionskrise"
"/cms/work/2006/45/515-WoWo-45"
"/cms/work/2006/45/516-Draussen"
"/cms/work/2006/45/516-Friedhof"
"/cms/work/2006/45/Anrei-er-oben-rechts"
"/cms/work/2006/45/Argument"
"/cms/work/2006/45/Aufgaben-Bundeswehr"
"/cms/work/2006/45/Autotest-Volvo-C70"
"/cms/work/2006/45/Badlands"
"/cms/work/2006/45/Bahn"
"/cms/work/2006/45/Barack-Obama"
"/cms/work/2006/45/BdW-45"
"/cms/work/2006/45/Beistueck-Leicht"
"/cms/work/2006/45/Berliner-Buehne"
"/cms/work/2006/45/Bischof-Huber"
"/cms/work/2006/45/Blocker-Elvis"
"/cms/work/2006/45/BU-Mur"
"/cms/work/2006/45/Burda-Bu"
"/cms/work/2006/45/Burda-Interview"
"/cms/work/2006/45/BU-s"
"/cms/work/2006/45/BU-Zeitreisen"
"/cms/work/2006/45/Camorra"
"/cms/work/2006/45/C-Campus-Washington"
"/cms/work/2006/45/c-gefragt-sprachlos"
"/cms/work/2006/45/C-Stillarbeit"
"/cms/work/2006/45/C-TuT45"
"/cms/work/2006/45/D-Aufmacher-45"
"/cms/work/2006/45/D-Empfehlungen-45"
"/cms/work/2006/45/D-Filmtitel-45"
"/cms/work/2006/45/D-Meinecke-45"
"/cms/work/2006/45/D-Musikklassiker-45"
"/cms/work/2006/45/D-Unteraufmacher-45"
"/cms/work/2006/45/England-Sprachkurs"
"/cms/work/2006/45/England-Sprachkurs-Info"
"/cms/work/2006/45/Finis-45"
"/cms/work/2006/45/Fortsetzungen"
"/cms/work/2006/45/Gewalt-45"
"/cms/work/2006/45/Glosse"
"/cms/work/2006/45/Glosse-1-45"
"/cms/work/2006/45/Grafiktext-1"
"/cms/work/2006/45/Grafiktext-2"
"/cms/work/2006/45/G-REITs"
"/cms/work/2006/45/Habermas"
"/cms/work/2006/45/Handgepaeck"
"/cms/work/2006/45/Headline-Huber"
"/cms/work/2006/45/Headline-PKV"
"/cms/work/2006/45/Headline-Schulz"
"/cms/work/2006/45/Hillary-Clinton"
"/cms/work/2006/45/Hoffnungen"
"/cms/work/2006/45/Hoffnungen-link"
"/cms/work/2006/45/Hogarth"
"/cms/work/2006/45/Impressum-seit-19-06"
"/cms/work/2006/45/inderzeit"
"/cms/work/2006/45/Infokasten"
"/cms/work/2006/45/Inhalt-aussen"
"/cms/work/2006/45/Inhalt-innen-1"
"/cms/work/2006/45/Inhalt-innen-2"
"/cms/work/2006/45/Inhalt--si"
"/cms/work/2006/45/Interview-Judt"
"/cms/work/2006/45/Interview-Saelzer"
"/cms/work/2006/45/Interview-Vladi"
"/cms/work/2006/45/Jelinek"
"/cms/work/2006/45/Kasten-Huber"
"/cms/work/2006/45/KJ-Heidelbach"
"/cms/work/2006/45/KJ-Julit"
"/cms/work/2006/45/KJ-Luchs-45"
"/cms/work/2006/45/KJ-Mozart"
"/cms/work/2006/45/KL-Buechertisch"
"/cms/work/2006/45/Kleiner-Text-45"
"/cms/work/2006/45/KL-Gedicht"
"/cms/work/2006/45/Klima"
"/cms/work/2006/45/Klima-Kasten"
"/cms/work/2006/45/KL-Mittelstueck"
"/cms/work/2006/45/KL-Stillleben"
"/cms/work/2006/45/Kolumne"
"/cms/work/2006/45/Komiker-Cohen"
"/cms/work/2006/45/Kongo-Wahlen"
"/cms/work/2006/45/Konjunktur"
"/cms/work/2006/45/Kunstfehler"
"/cms/work/2006/45/Kunstmarkt-6-Fragen"
"/cms/work/2006/45/Kunstmarkt-Aufgerufen"
"/cms/work/2006/45/Kunstmarkt-Aufmacher"
"/cms/work/2006/45/Kunstmarkt-Kommentar"
"/cms/work/2006/45/LB45-beilage"
"/cms/work/2006/45/LB45-DOSSIERkevin"
"/cms/work/2006/45/LB45-POLarmut"
"/cms/work/2006/45/LB45-POLm-ntefering"
"/cms/work/2006/45/LB45-POLmuslime"
"/cms/work/2006/45/LB45-WISSENexzellenz"
"/cms/work/2006/45/LB45-ZLitalien"
"/cms/work/2006/45/LB45-ZLk-penickiade"
"/cms/work/2006/45/L-Interview-Biermann"
"/cms/work/2006/45/Lit-Inhalt"
"/cms/work/2006/45/L-Krimi-Schaedel"
"/cms/work/2006/45/LS-Duisburg"
"/cms/work/2006/45/LS-Holocaustmuseum"
"/cms/work/2006/45/Macher-Maerkte"
"/cms/work/2006/45/Maeusekino-45"
"/cms/work/2006/45/Maeusekino-Wirtschaft"
"/cms/work/2006/45/MarieAntoinette"
"/cms/work/2006/45/Markt-45"
"/cms/work/2006/45/Martenstein-45"
"/cms/work/2006/45/Meldungen-45"
"/cms/work/2006/45/Mexiko"
"/cms/work/2006/45/Mexiko-Info"
"/cms/work/2006/45/M-Tuberkulose"
"/cms/work/2006/45/N-Honig"
"/cms/work/2006/45/N-Kennzeichnung-Kasten"
"/cms/work/2006/45/Ohio"
"/cms/work/2006/45/P-Angst"
"/cms/work/2006/45/Pariser-Vorstadtkrawalle"
"/cms/work/2006/45/Paulson"
"/cms/work/2006/45/Paulson-Kasten"
"/cms/work/2006/45/P-Eulenburg"
"/cms/work/2006/45/PKV"
"/cms/work/2006/45/P-Leinfelder"
"/cms/work/2006/45/P-Leinfelder-Kasten"
"/cms/work/2006/45/Populismus-Slowakei"
"/cms/work/2006/45/P-Sturm"
"/cms/work/2006/45/Rihm-Oper"
"/cms/work/2006/45/Rostock"
"/cms/work/2006/45/Schulz"
"/cms/work/2006/45/Sehenswert45"
"/cms/work/2006/45/Siebeck-K-chenschrank-Pinselglas"
"/cms/work/2006/45/Siebeck-Kolumne-Buecher-II"
"/cms/work/2006/45/Snowcake"
"/cms/work/2006/45/Spielen-Ecke45"
"/cms/work/2006/45/Spielen-Lebensgeschichte-45"
"/cms/work/2006/45/Spielen-Logelei-45"
"/cms/work/2006/45/Spielen-Schach-45"
"/cms/work/2006/45/Spielen-Scrabble45"
"/cms/work/2006/45/Spielen-Sudoku45"
"/cms/work/2006/45/Spitze-45"
"/cms/work/2006/45/Stimmts-Pestizid"
"/cms/work/2006/45/ST-Zizek-45"
"/cms/work/2006/45/T-AutoAuto"
"/cms/work/2006/45/Text-Logo"
"/cms/work/2006/45/T-Galileo"
"/cms/work/2006/45/Titel-Ank-ndigung-45"
"/cms/work/2006/45/Titel-Computerspiele-45"
"/cms/work/2006/45/Traum-Woody-Allen"
"/cms/work/2006/45/T-Technik-Kasten"
"/cms/work/2006/45/Tuerkei"
"/cms/work/2006/45/US-Irak-Politik"
"/cms/work/2006/45/Vitakasten"
"/cms/work/2006/45/Vita-Schmidt"
"/cms/work/2006/45/Vordenker-Serie"
"/cms/work/2006/45/Vorlesegeschichte"
"/cms/work/2006/45/v_-Zeit-Mitarbeitern"
"/cms/work/2006/45/Wahlen-USA"
"/cms/work/2006/45/Welt-in-Zahlen"
"/cms/work/2006/45/Westerwelle"
"/cms/work/2006/45/W-Geschichten-45"
"/cms/work/2006/45/W-Gesellschafter-45"
"/cms/work/2006/45/W-Holzamer-45"
"/cms/work/2006/45/Wilhelm-Heitmeyer"
"/cms/work/2006/45/W-Liste-Unterschriften-45"
"/cms/work/2006/45/Wowos-Zsp_-45"
"/cms/work/2006/45/W-UnsereKleineWelt-45"
"/cms/work/2006/45/W-Zettel-45"
"/cms/work/2006/45/ZB-Hexenjagd"
"/cms/work/2006/45/ZEIT-Campus"
"/cms/work/2006/45/Zeit-online"
"/cms/work/2006/45/Zinsen"
"/cms/work/2006/45/Zitat"))

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
