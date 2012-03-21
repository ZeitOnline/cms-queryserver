;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; indent-tabs-mode:nil; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:     db-poo.lisp
;;;; Purpose:  Pooled Database Connections
;;;; Author:   Ralf Mattes
;;;; Created:  2007-09-04
;;;; Version:  $Id$
;;;; Copyright 2007 Ralf Mattes
;;;;           Full licence at end of file.
;;;; Commentary:
;;;;
;;;;
;;;;
;;;; *************************************************************************


(in-package :cms-query)

(defclass resource-pool ()
  ((resources
      :accessor resources-of
      :initform ())
   (res-count
      :accessor res-count-of :initform 0)
   (min-resources
      :accessor min-resources-of
      :initarg  :min-resources)
   (max-resources
      :accessor max-resources-of
      :initarg  :max-resources)
   (mutex
      :accessor pool-mutex)
   (resource-factory
      :accessor factory
      :initarg  :factory)
   (resource-shredder
      :accessor resource-shredder
      :initarg  :resource-shredder
      :initform NIL)
   (userdata
      :accessor userdata-of
      :initarg
      :user-data
      :initform NIL))

  (:documentation "Basic resource pool implementation."))

;;; Here follows the public API of pooled resouce classes

(defgeneric fetch-resource (resource-pool))

(defgeneric release-resource (resource-pool resource))

(defgeneric dump-resource (resource-pool resource))

(defgeneric clear-pool (resource-pool))

;;; Pooled database connections
;;; The userdata slot holds the connection string

(defclass database-pool (resource-pool)
())

(defmethod initialize-instance :after ((pool resource-pool) &key &allow-other-keys)
  ;;; Create the mutex
  (setf (pool-mutex pool) (tbnl::make-lock (symbol-name (gensym "Database Pool Lock "))))
  ;;; Initialize the resource pool
  (tbnl::with-lock ((pool-mutex pool)) 
    (dotimes (i (min-resources-of pool))
      (push (clsql:connect (userdata-of pool) :if-exists :new :make-default NIL) 
            (resources-of pool))
      (incf (res-count-of pool)))    
    pool))

(defmethod fetch-resource ((pool database-pool))
  (tbnl::with-lock ((pool-mutex pool))
    ;;; FIXME: do we need an unwind-protect here?
    (decf (res-count-of pool))
    (pop (resources-of pool))))

;;; FIXME: needs locking/mutex!
(defmethod fetch-resource :before ((pool database-pool))
  (let ((res-count (length (resources-of pool)))
        (min-count  (min-resources-of pool)))
    (when (< res-count min-count)
      (warn "Pool underrun! Increasing the wee")
      (dotimes (i (- min-count res-count))
        (push (clsql:connect (userdata-of pool) :if-exists :new :make-default NIL)
              (resources-of pool))
        (incf (res-count-of pool))))))

(defmethod release-resource ((pool database-pool) resource)
;;  (tbnl::with-lock ((pool-mutex pool)))
  (unless (member resource (resources-of pool)))
  (push resource (resources-of pool))
  (values))

(defmethod clear-pool ((pool database-pool))
  (tbnl::with-lock ((pool-mutex pool))
    (loop for resource in (resources-of pool) do (dump-resource pool resource))
    (values)))

;;; TODO
;;; 
;;; * We should'nt call clsql:connect directly but rather call the factory
;;;   closure.

 
;;;; *************************************************************************
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 2, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
