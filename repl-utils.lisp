;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     repl-utils.lisp
;;;; Purpose:  Helpful utilities to use CMS-QUERY from a REPL
;;;; Author:   Ralf Mattes 
;;;; Created:  2008-07-03
;;;; Version:  $Id$
;;;; Copyright 2008 Ralf Mattes
;;;;           Full licence at end of file.
;;;; Commentary:
;;;;
;;;; 
;;;;
;;;; *************************************************************************


(in-package :cms-query-user)

(defun query-for-query ()
  ""
  (format T "Please enter a CMS query: ")
  (read))

(defun debug-query (&optional query-form)
  "Compile a CMS query form and return the resulting raw SQL query"
  (let* ((query (compile-query (or query-form (query-for-query))))
         (ssql  (cms-query::compile-sql query))
         (sql   (s-sql:sql-compile ssql))
         (elisp (format nil  "(x-set-selection nil ~S)" sql)))
    (swank::eval-in-emacs elisp)
    (warn sql)
    (values)))


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
