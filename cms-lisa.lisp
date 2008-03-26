;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     cms-lisa.lisp
;;;; Purpose:  Interface between the CMS and the LISA engine
;;;; Author:   Ralf Mattes 
;;;; Created:  2008-03-26
;;;; Version:  $Id$
;;;; Copyright 2008 Ralf Mattes
;;;;           Full licence at end of file.
;;;; Commentary:
;;;;
;;;; 
;;;;
;;;; *************************************************************************


(in-package :cms-query)

(require :lisa)
(use-package :lisa)

;;; A generic RDF triple (with name in Clark-notation)
(deftemplate triple () 
  (slot uri) 
  (slot name)
  (slot value))

(defrule rule-on-status-ok ()
  (triple (uri ?uri) (name "status") (value "OK"))
  =>
  (format T "Resource ~A status changed to OK -> publish it~%" ?uri))

(defun study-resource (resource-uri) 
  )

(defun check-rules (resource-uri)
  (reset)
  (study-resource resource-uri)
  
  )
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
