(in-package :cl-user)

(defpackage :plot2d
  (:use :common-lisp
        :cairo)
  (:export :plot
           :plot/xy
           :plot/polar+a))

(in-package :plot2d)

(defparameter *context* nil)

(defun integers-between (x y)
  (loop for i from (ceiling x) to (floor y) collect i))

(defun half-ints-between (x y)
  (remove-if #'(lambda (n) (or (< n x) (> n y))) 
             (loop for i from (floor x) to (ceiling y) by 0.5 collect i)))

(defun thin (n l)
  (loop for i from 0 to (1- (length l)) by (ceiling (/ (length l) n)) collect (nth i l)))

(defun plot/xy (xvals yvals &key (aspect 1.0) (background (list 0.2 0.2 0.2)) (palette '((1.0 0.2 0.2) (0.2 1.0 0.2) (0.2 0.2 1.0)))
                     (legend nil) (labels nil) (width 800) (filename "plot2d.pdf") (format :pdf))
  (let* ((xvals (if (listp (car xvals)) xvals (list xvals)))
         (yvals (if (listp (car yvals)) yvals (list yvals)))
         (colors (loop for x in xvals appending (loop for y in palette collect y)))
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
      (apply #'set-source-rgb background)
      (paint)
      (set-line-width 1)
      ;; draw axis
      (set-source-rgb 0.5 0.5 0.5)
      (move-to (tx minx) (ty miny))
      (line-to (tx minx) (ty maxy))
      (line-to (tx maxx) (ty maxy))
      (line-to (tx maxx) (ty miny))
      (line-to (tx minx) (ty miny))
      (stroke)

      ;; draw ticks along x axis
      (set-font-size 12)
      (loop for x in (thin 10 (integers-between minx maxx))
           do (progn
                (move-to (tx x) (+ (ty miny) 6))
                (line-to (tx x) (- (ty miny) 6))
                (stroke)

                (move-to (tx x) (+ (ty maxy) 6))
                (line-to (tx x) (- (ty maxy) 6))
                (stroke)

                (move-to (- (tx x) 6) (+ (ty miny) 18))
                (show-text (format nil "~A" (floor x)))))

      ;; sub-ticks
      (when (<= (length (half-ints-between minx maxx)) 20)
        (loop for x in (half-ints-between minx maxx)
           do (progn
                (move-to (tx x) (+ (ty miny) 3))
                (line-to (tx x) (- (ty miny) 3))
                (stroke)
                
                (move-to (tx x) (+ (ty maxy) 3))
                (line-to (tx x) (- (ty maxy) 3))
                (stroke))))
        
      ;; draw ticks along y axis
      (loop for y in (thin 10 (integers-between miny maxy))
           do (progn
                (move-to (+ (tx minx) 6) (ty y))
                (line-to (- (tx minx) 6) (ty y))
                (stroke)
                (move-to (+ (tx maxx) 6) (ty y))
                (line-to (- (tx maxx) 6) (ty y))
                (stroke)

                (move-to (- (tx minx) 22) (ty y))
                (show-text (format nil "~A" (floor y)))))

      ;; sub-ticks
      (when (<= (length (half-ints-between miny maxy)) 20)
        (loop for y in (half-ints-between miny maxy)
           do (progn
                (move-to (+ (tx minx) 3) (ty y))
                (line-to (- (tx minx) 3) (ty y))
                (stroke)
                (move-to (+ (tx maxx) 3) (ty y))
                (line-to (- (tx maxx) 3) (ty y))
                (stroke))))

      ;; label the x and y axis
      (when labels
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
        (let ((bz 30)
              (bg (append (mapcar #'(lambda (x) (- 1.0 x)) background) '(0.8))))
          (destructuring-bind (xb yb w h) (max-extents legend)
            (let ((h (+ 5 h)))
              (apply #'set-source-rgba bg)
              (rectangle (+ bz bx -5 xb) (+ yb by bz -5) (+ w 75) (+ 10 (* h (length legend))))
              (fill-path)
              (loop for txt in legend
                 for c in colors
                 as y = (+ bz by) then (+ y h) do
                   (apply #'set-source-rgb background)
                   (move-to (+ bx bz) y)
                   (show-text txt)
                   (move-to (+ bx bz 5 w xb) (+ (/ yb 2) y))
                   (apply #'set-source-rgb c)
                   (line-to (+ bx bz 65 w xb) (+ (/ yb 2) y))
                   (stroke)))))))
                
    (destroy *context*)))

(defun plot (funcs &key (x-axis (list -2 2)) (aspect 1.0) (samples 100) (background (list 0.2 0.2 0.2)) (palette '((1.0 0.2 0.2) (0.2 1.0 0.2) (0.2 0.2 1.0)))
                     (legend nil) (labels nil) (width 800) (filename "plot2d.pdf") (format :pdf))
  (let* ((funcs (if (listp funcs) funcs (list funcs)))
         (dx (- (second x-axis) (first x-axis)))
         (vals (loop for f in funcs collect
                    (loop for x from (first x-axis) to (second x-axis) by (/ dx samples)
                       collect (list x (funcall f x)))))
         (xvals (loop for xv in vals
                   collect (loop for x in xv collect (first x))))
         (yvals (loop for yv in vals
                   collect (loop for y in yv collect (second y)))))
    (plot-xy xvals yvals :aspect aspect :background background :palette palette :legend legend :labels labels :width width :filename filename :format format)))


(defun plot/polar+a (func &key (theta-range (list 0 (+ pi pi))) (a-values '(1.0)) (aspect 1.0) (samples 200) (background (list 0.2 0.2 0.2)) (palette '((1.0 0.2 0.2) (0.2 1.0 0.2) (0.2 0.2 1.0)))
                     (legend nil) (labels nil) (width 800) (filename "plot2d.pdf") (format :pdf))
  "Plot a polar-coordinate function parameterized by a. `FUNC` takes two arguments, theta and a."
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
    (destructuring-bind (p q) (loop for a in a-values
                                 as (x y) = (func/ar a func)
                                 collecting x into xv
                                 collecting y into yv
                                 finally (return (list xv yv)))
      (plot/xy p q 
               :aspect aspect
               :background background
               :palette palette
               :legend legend
               :labels labels
               :width width
               :filename filename
               :format format))))

    
  
