(in-package :cl-user)

(defpackage :plot2d.theme
  (:use :common-lisp))

(in-package :plot2d.theme)

(defclass theme ()
    ((background :accessor background :initarg :bg :initform '(0 0 0))
     (palette :accessor palette :initarg :palette :initform (list '(1 0 0)
                                                                  '(0 1 0)
                                                                  '(0 0 1)))
     (axis-color :initarg :axis-color :accessor axis-color :initform '(.5 .5 .5))
     (axis-font-size :initarg :axis-font-size :accessor axis-font-size :initform 12)
     (label-color :initarg :label-color :accessor label-color :initform '(.5 .5 .5))
     (label-face :initarg :label-face :accessor label-face :initform nil)
     (label-font-size :initarg :label-font-size :accessor label-font-size :initform 12)
     (legend-color :initarg :legend-color :accessor legend-color :initform nil)
     (legend-face :initarg :legend-face :accessor legend-face :initform nil)
     (legend-font-size :initarg :legend-font-size :accessor legend-font-size :initform 12)
     (legend-font-color :initarg :legend-font-color :accessor legend-font-color :initform nil)
     (legend-alpha :initarg :legend-alpha :accessor legend-alpha :initform .8)
     (legend-placement :initarg :legend-placement :accessor legend-placement :initform '(30 30))))
  
(defparameter *themes* (make-hash-table :test #'equal))

      
