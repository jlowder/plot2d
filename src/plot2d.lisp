(in-package :cl-user)

(defpackage :plot2d
  (:use :common-lisp
        :cairo)
  (:import-from :plot2d.theme
                :theme
                :background
                :axis-color
                :axis-font-size
                :label-color
                :label-face
                :label-font-size
                :legend-color
                :legend-face
                :legend-font-size
                :legend-alpha
                :legend-placement)
  (:export :plot
           :plot/
           :plot/xy
           :plot/polar
           :plot/polar+a
           :plot/polar+xy
           :plot/polar+xya
           :plot2d))

(in-package :plot2d)

(defparameter *context* nil)

(defun thin (n l)
  (when l
    (loop for i from 0 to (1- (length l)) by (ceiling (/ (length l) n)) collect (nth i l))))

(defun ticks (low high)
  (let* ((d (- high low))
         (decades (expt 10 (1- (round (log d 10)))))
         (nl (* decades (floor (/ low decades)))))
    (mapcar #'(lambda (x) (coerce x 'float))
            (remove-if #'(lambda (x) (or (< x low) (> x high)))
                       (loop for x from nl to (+ high decades) by decades collect x)))))

(defun subticks (low high)
  (let* ((d (- high low))
         (decades (expt 10 (1- (round (log d 10)))))
         (nl (* decades (floor (/ low decades)))))
    (mapcar #'(lambda (x) (coerce x 'float))
            (remove-if #'(lambda (x) (or (< x low) (> x high)))
                       (loop for x from (- nl (/ decades 2)) to (+ high decades) by decades collect x)))))

(defun plot/xy (xvals yvals theme aspect legend labels width filename format)
  "Create a 2D plot. `XVALS` is a list of X coordinates, and `YVALS` is a list of corresponding Y coordinate values. They can also be a list of lists
if multiple curves are being plotted."
  (let* ((xvals (if (listp (car xvals)) xvals (list xvals)))
         (yvals (if (listp (car yvals)) yvals (list yvals)))
         (colors (loop for x in xvals appending (loop for y in (palette theme) collect y)))
         (vals (loop
                  for xlist in xvals
                  for ylist in yvals
                  collect (loop 
                             for x in xlist
                             for y in ylist
                             collect (list x y))))
         (maxx (loop for i in vals
                  maximizing (apply #'max (loop for x in i collect (first x))) into z finally (return z)))
         (minx (loop for i in vals
                  minimizing (apply #'min (loop for x in i collect (first x))) into z finally (return z)))
         (maxy (loop for i in vals
                  maximizing (apply #'max (loop for x in i collect (second x))) into z finally (return z)))
         (miny (loop for i in vals
                  minimizing (apply #'min (loop for x in i collect (second x))) into z finally (return z)))
         (height (/ width aspect))
         (bx 50)
         (by 50)
         (rx (/ (- width bx bx) width))
         (ry (/ (- height by by) height))
         (surface
          (cond 
            ((eq format :pdf) (create-pdf-surface filename width height))
            ((eq format :svg) (create-svg-surface filename width height))
            ((eq format :ps) (create-ps-surface filename width height))
            (t (error "invalid file format")))))
    (flet ((tx (x)
             (+ bx (* rx (/ (- x minx) (- maxx minx))
                      width)))
           (ty (y)
             (- height (+ by (* ry (/ (- y miny) (- maxy miny))
                      height))))
           (max-extents (l)
             (let ((cnv (mapcar #'(lambda (x) (multiple-value-list (text-extents x))) l)))
               (loop for i from 0 to 3 collect
                    (loop for x in cnv maximizing (nth i x) into z finally (return z))))))
                  
      (setf *context* (create-context surface))
      (destroy surface)
      (apply #'set-source-rgb (background theme))
      (paint)
      (set-line-width 1)
      ;; draw axis
      (apply #'set-source-rgb (axis-color theme))
      (move-to (tx minx) (ty miny))
      (line-to (tx minx) (ty maxy))
      (line-to (tx maxx) (ty maxy))
      (line-to (tx maxx) (ty miny))
      (line-to (tx minx) (ty miny))
      (stroke)

      ;; draw ticks along x axis
      (set-font-size (axis-font-size theme))
      (loop for x in (ticks minx maxx)
         do (progn
              (move-to (tx x) (+ (ty miny) 0))
              (line-to (tx x) (- (ty miny) 6))
              (stroke)
              
              (move-to (tx x) (+ (ty maxy) 6))
              (line-to (tx x) (- (ty maxy) 0))
              (stroke)))
      
      (loop for x in (thin 10 (ticks minx maxx))
         do (progn
              (move-to (- (tx x) 6) (+ (ty miny) 18))
              (show-text (format nil "~A" x))))
      
      ;; sub-ticks
      (when (<= (length (ticks minx maxx)) 15)
        (loop for x in (subticks minx maxx)
           do (progn
                (move-to (tx x) (+ (ty miny) 0))
                (line-to (tx x) (- (ty miny) 3))
                (stroke)
                
                (move-to (tx x) (+ (ty maxy) 3))
                (line-to (tx x) (- (ty maxy) 0))
                (stroke))))
      
      ;; draw ticks along y axis
      (loop for y in (ticks miny maxy)
         do (progn
              (move-to (+ (tx minx) 6) (ty y))
              (line-to (- (tx minx) 0) (ty y))
              (stroke)
              (move-to (+ (tx maxx) 0) (ty y))
              (line-to (- (tx maxx) 6) (ty y))
              (stroke)))
      
      (loop for y in (thin (/ 10 aspect) (ticks miny maxy))
         do (progn
              (move-to (- (tx minx) 35) (ty y))
              (show-text (format nil "~A" y))))
      
      ;; sub-ticks
      (when (<= (length (ticks miny maxy)) (/ 15 aspect))
        (loop for y in (subticks miny maxy)
           do (progn
                (move-to (+ (tx minx) 3) (ty y))
                (line-to (- (tx minx) 0) (ty y))
                (stroke)
                (move-to (+ (tx maxx) 0) (ty y))
                (line-to (- (tx maxx) 3) (ty y))
                (stroke))))
      
      ;; label the x and y axis
      (when labels
        (set-font-size (label-font-size theme))
        (destructuring-bind (xname yname) labels
          (multiple-value-bind (xb yb w h) (text-extents xname)
            (move-to (- (+ xb (/ width 2)) (/ w 2)) (- height 5))
            (show-text xname))
          (multiple-value-bind (xb yb w h) (text-extents yname)
            (move-to (+ 5 h) (+ (/ w 2) (/ height 2)))
            (translate 0 0)
            (rotate (/ pi -2))
            (show-text yname)
            (reset-trans-matrix))))

      ;; draw graph
      (set-line-width 2)
      (loop for p in vals
            for c in colors do
           (apply #'set-source-rgb c)
           (loop for (x y) in p
              as flag = t then nil
              do (if flag
                     (move-to (tx x) (ty y))
                     (line-to (tx x) (ty y))))
           (stroke))
    
      ;; draw legend
      (when legend
        (set-line-width 1)
        (set-font-size (legend-font-size theme))
        (let ((bz 30)
              (bg (append 
                   (if (legend-color theme)
                       (legend-color theme)
                       (mapcar #'(lambda (x) (- 1.0 x)) (background theme)))
                   (list (legend-alpha theme)))))
          (destructuring-bind (xb yb w h) (max-extents legend)
            (let ((h (+ 5 h)))
              (apply #'set-source-rgba bg)
              (rectangle (+ bz bx -5 xb) (+ yb by bz -5) (+ w 75) (+ 10 (* h (length legend))))
              (fill-path)
              (loop for txt in legend
                 for c in colors
                 as y = (+ bz by) then (+ y h) do
                   (apply #'set-source-rgb (background theme))
                   (move-to (+ bx bz) y)
                   (show-text txt)
                   (move-to (+ bx bz 5 w xb) (+ (/ yb 2) y))
                   (apply #'set-source-rgb c)
                   (line-to (+ bx bz 65 w xb) (+ (/ yb 2) y))
                   (stroke)))))))
                
    (destroy *context*)))

(defun plot/ (funcs x-axis theme aspect samples legend labels width filename format)
  "Plot Y as a funcation of X. `FUNCS` should be a function for Y given X, or a list of such functions."
  (let* ((funcs (if (listp funcs) funcs (list funcs)))
         (dx (- (second x-axis) (first x-axis)))
         (vals (loop for f in funcs collect
                    (loop for x from (first x-axis) to (second x-axis) by (/ dx samples)
                       collect (list x (funcall f x)))))
         (xvals (loop for xv in vals
                   collect (loop for x in xv collect (first x))))
         (yvals (loop for yv in vals
                   collect (loop for y in yv collect (second y)))))
    (plot/xy xvals yvals theme aspect legend labels width filename format)))

(defun plot/polar (funcs theta-range theme aspect samples legend labels width filename format)
  "Plot a polar-coordinate function (or list of functions). Each function in `FUNCS` takes one arguments, theta."
  (flet ((func/r (f)
           (let* ((theta (loop for x from (first theta-range) to (second theta-range) by (/ (- (second theta-range) (first theta-range)) samples) collect x))
                  (r (loop for n in theta collecting (funcall f n))))
             (list
              (loop
                 for th in theta
                 for rv in r
                 collect (* rv (cos th)))
              (loop
                 for th in theta
                 for rv in r
                 collect (* rv (sin th)))))))
    (let ((funcs (if (listp funcs) funcs (list funcs))))
      (destructuring-bind (x y)
          (loop for f in funcs
             as (p q) = (func/r f)
             appending p into xp
             appending q into yq
             finally (return (list xp yq)))
        (plot/xy x y theme aspect legend labels width filename format)))))

(defun plot/polar+a (funcs theta-range theme a-values aspect samples legend labels width filename format)
  "Plot a polar-coordinate function (or list of functions) parameterized by a. `FUNCS` takes two arguments, theta and a."
  (flet ((func/ar (a f)
           (let* ((theta (loop for x from (first theta-range) to (second theta-range) by (/ (- (second theta-range) (first theta-range)) samples) collect x))
                  (r (loop for n in theta collecting (funcall f n a))))
             (list
              (loop
                 for th in theta
                 for rv in r
                 collect (* rv (cos th)))
              (loop
                 for th in theta
                 for rv in r
                 collect (* rv (sin th)))))))
    (let ((funcs (if (listp funcs) funcs (list funcs))))
      (destructuring-bind (x y)
          (loop for f in funcs
             as (p q) = (loop for a in a-values
                           as (x y) = (func/ar a f)
                           collecting x into xv
                           collecting y into yv
                           finally (return (list xv yv)))
             appending p into xp
             appending q into yq
             finally (return (list xp yq)))
        (plot/xy x y theme aspect legend labels width filename format)))))

(defun plot/polar+xya (funcs theta-range theme a-values aspect samples legend labels width filename format)
  "Plot functions of X and Y parameterized by a. `FUNCS` takes two arguments, theta and a. Each curve should be a pair of functions."
  (flet ((func/a (a f)
           (let* ((theta (loop for x from (first theta-range) to (second theta-range) by (/ (- (second theta-range) (first theta-range)) samples) collect x)))
             (loop for n in theta collecting (funcall f n a)))))
    (destructuring-bind (xfuncs yfuncs) 
        (loop for f in funcs
           as i = 0 then (1+ i)
           if (evenp i)
           collect f into a
           else collect f into b
           end finally (return (list a b)))
      (destructuring-bind (x y)
          (loop for a in a-values
             as xl = (mapcar #'(lambda (x) (func/a a x)) xfuncs)
             as yl = (mapcar #'(lambda (x) (func/a a x)) yfuncs)
             appending xl into xp
             appending yl into yp
             finally (return (list xp yp)))
        (plot/xy x y theme aspect legend labels width filename format)))))

(defun plot/polar+xy (funcs theta-range theme aspect samples legend labels width filename format)
  "Plot polar functions of X and Y. Each `FUNCS` takes one argument, theta. Each curve should be a pair of functions."
  (flet ((func/r (f)
           (let* ((theta (loop for x from (first theta-range) to (second theta-range) by (/ (- (second theta-range) (first theta-range)) samples) collect x)))
             (loop for n in theta collecting (funcall f n)))))
    (destructuring-bind (xfuncs yfuncs) 
        (loop for f in funcs
           as i = 0 then (1+ i)
           if (evenp i)
           collect f into a
           else collect f into b
           end finally (return (list a b)))
      (destructuring-bind (x y)
          (list (mapcar #'(lambda (x) (func/r x)) xfuncs)
                (mapcar #'(lambda (x) (func/r x)) yfuncs))
        (plot/xy x y theme aspect legend labels width filename format)))))

(defclass plot2d ()
  ((theme :accessor theme :initarg :theme :initform (make-instance 'theme))
   (width :accessor width :initarg :width :initform 800)
   (aspect :accessor aspect :initarg :aspect :initform 1)
   (format :accessor get-format :initarg :format :initform 'pdf)
   (labels :accessor get-labels :initarg :labels :initform nil)
   (legend :accessor legend :initarg :legend :initform nil)
   (samples :accessor samples :initarg :samples :initform 200)
   (range :accessor range :initarg :range :initform '(-2 2))))

(defclass parameterized (plot2d)
  ((a-values :accessor a-values :initarg :a-values :initform nil)))

(defclass polar-r (plot2d) ())

(defclass polar-ra (parameterized) ())

(defclass polar-xy (plot2d) ())

(defclass polar-xya (parameterized) ())
  
(defgeneric generate (gen funcs))

(defmethod generate ((gen plot2d) funcs)
  (plot/ funcs (range gen) (theme gen) (aspect gen) (samples gen) (legend gen) (get-labels gen) (width gen) (filename gen) (get-format gen)))

(defmethod generate ((gen polar-r) funcs)
  (plot/polar funcs (range gen) (theme gen) (aspect gen) (samples gen) (legend gen) (get-labels gen) (width gen) (filename gen) (get-format gen)))

(defun plot (gen funcs &key filename width theme range aspect format labels legend)
  (when width
    (setf (width gen) width))
  (when range
    (setf (range gen) range))
  (when theme
    (setf (theme gen) theme))
  (when filename
    (setf (filename gen) filename))
  (when format
    (setf (format gen) format))
  (when aspect
    (setf (aspect gen) aspect))
  (when labels
    (setf (get-labels gen) labels))
  (when legend
    (setf (legend gen) legend))
  (generate gen funcs))
