;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     utils.lisp
;;;; Purpose:  Utility functions useful for development
;;;; Author:   Ralf Mattes 
;;;; Created:  2007-09-10
;;;; Version:  $Id$
;;;; Copyright 2007 Ralf Mattes
;;;;           Full licence at end of file.
;;;; Commentary:
;;;;
;;;; 
;;;;
;;;; *************************************************************************


(in-package :cms-query-server)

(defun be-verbose! ()
  (setf tbnl:*show-lisp-backtraces-p* t)
  (setf tbnl:*show-lisp-errors-p* t))

(defun shut-up! ()
  (setf tbnl:*show-lisp-backtraces-p* nil)
  (setf tbnl:*show-lisp-errors-p* nil))

(defmacro defformatter (name &body body)
  `(setf (symbol-function ',name) ,@body))

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
