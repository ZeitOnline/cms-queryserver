;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: REZENSIONEN -*-
;;; CVS-ID: $Id: run-webgui.lisp 113 2006-03-22 16:16:11Z rmattes $


(dribble "/usr/local/share/cms-query/dribble.file")
(sb-sys:ignore-interrupt SB-UNIX:SIGPIPE)
(pushnew #p"/usr/local/share/LISP/systems/" asdf:*central-registry*)
(pushnew #p"/usr/local/src/CMS-DB-QUERY/" asdf:*central-registry*)
(pushnew #p"/usr/local/src/CMS-DB-QUERY/query-server/" asdf:*central-registry*)


(asdf:oos 'asdf:compile-op :cms-query)
(asdf:oos 'asdf:load-op :cms-query)
(in-package :cms-query)
(clsql:connect '("localhost" "cms" "postgres" "") :pool t)
(start :address "10.100.10.27" :port 9999) 



