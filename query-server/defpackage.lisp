;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     defpackage.lisp
;;;; Purpose:  Defines package for cms-query-server
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


(in-package :cl)

(defpackage #:cms-query-server 
  (:use #:cl #:cms-query #:hunchentoot)
  (:export
   #:start
   #:stop
   #:list-connections
   ))

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
