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
     (label-font-size :initarg :label-font-size :accessor label-font-size :initform 14)
     (legend-color :initarg :legend-color :accessor legend-color :initform nil)
     (legend-face :initarg :legend-face :accessor legend-face :initform nil)
     (legend-font-size :initarg :legend-font-size :accessor legend-font-size :initform 12)
     (legend-font-color :initarg :legend-font-color :accessor legend-font-color :initform nil)
     (legend-alpha :initarg :legend-alpha :accessor legend-alpha :initform .8)
     (legend-placement :initarg :legend-placement :accessor legend-placement :initform '(-60 30))))
  
(defparameter *themes* (make-hash-table :test #'equal))

(defun cvg (n)
  "This is a convenience function to assist converting emacs themes"
  (mapcar #'(lambda (x) (coerce (/ x 255) 'float)) (list
                                                    (ash (logand n #xff0000) -16)
                                                    (ash (logand n #x00ff00) -8)
                                                    (logand n #x0000ff))))

(setf (gethash "zenburn" *themes*)
      (make-instance 'theme
                     :bg (cvg #x3f3f3f)
                     :palette (mapcar #'cvg
                                      '(#xcc9393
                                        #xdfaf8f
                                        #x7f9f7f
                                        #x93e0e3
                                        #x8cd0d3
                                        #xdc8cc3))
                     :axis-color (cvg #xdcdccc)
                     :label-color (cvg #xdcdccc)
                     :legend-alpha .9
                     :legend-color '(0 0 0)
                     :legend-font-color (cvg #xdcdccc)
                     :label-font-size 16))

(setf (gethash "solarized" *themes*)
      (make-instance 'theme
                     :bg (cvg #x52676f)
                     :palette (mapcar #'cvg
                                      '(#x728a05
                                        #xbd3612
                                        #xc60007
                                        #xc61b6e
                                        #x5859b7
                                        #x2075c7))
                     :axis-color '(0 0 0)
                     :label-color '(0 0 0)
                     :legend-alpha .8
                     :legend-font-color '(0 0 0)
                     :label-font-size 16))

(setf (gethash "github" *themes*)
      (make-instance 'theme
                     :bg (cvg #xf8f8ff)
                     :palette (mapcar #'cvg
                                      '(#x009926
                                        #xbcd5fa
                                        #x445588
                                        #xdd1144
                                        #x990073
                                        #xeeeeee))
                     :axis-color '(0 0 0)
                     :label-color '(0 0 0)
                     :legend-alpha .8
                     :legend-font-color '(1 1 1)
                     :label-font-size 16))

(setf (gethash "subatomic" *themes*)
      (make-instance 'theme
                     :bg (cvg #x303347)
                     :palette (mapcar #'cvg
                                      '(#x696e92
                                        #x8aa6bc
                                        #xa9dc69
                                        #xf9b529
                                        #x9c71a5
                                        #xea8673))
                     :axis-color (cvg #xe5e5e5)
                     :label-color (cvg #xe5e5e5)
                     :legend-alpha .9
                     :label-font-size 16))


                     
