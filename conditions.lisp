;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     conditions.lisp
;;;; Purpose:  Define common conditions for the CMS-QUERY system
;;;; Author:   Ralf Mattes 
;;;; Created:  2008-01-22
;;;; Version:  $Id$
;;;; Copyright 2008 Ralf Mattes
;;;;           Full licence at end of file.
;;;; Commentary:
;;;;
;;;; 
;;;;
;;;; *************************************************************************


(in-package :cms-query)

(define-condition cms-query-error ()
  ((explanation :initarg :explanation :accessor explanation))
  (:report (lambda (c s)
	     (format s "~A" (explanation c)))))

(define-condition cms-malformed-query (cms-query-error)
  ((opcode :initarg opcode)))

(define-condition cms-unsupported-query (cms-query-error)
  ())

(define-condition invalid-namespace-prefix ()
  ((prefix :initarg :prefix :reader namespace-prefix))
  (:report (lambda (c s)
             (format s "No matching namespace found for prefix '~a'" (prin1-to-string
								 (namespace-prefix c))))))

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
