(ql:quickload :cl-cairo2)
(use-package :cairo)

(defparameter *context* nil)
(defparameter *width* 800)

(defun integers-between (x y)
  (loop for i from (ceiling x) to (floor y) collect i))

(defun thin (n l)
  (loop for i from 0 to (1- (length l)) by (ceiling (/ (length l) n)) collect (nth i l)))

(defun plot2d (funcs &key (x-axis (list -2 2)) (aspect 1.0) (samples 100) (background (list 0.2 0.2 0.2)) (foreground (list 1 1 1)) (plot-colors '((1.0 0.2 0.2) (0.2 1.0 0.2) (0.2 0.2 1.0))))
  (let* ((funcs (if (listp funcs) funcs (list funcs)))
         (colors (loop for x in funcs appending (loop for y in plot-colors collect y)))
         (dx (- (second x-axis) (first x-axis)))
         (height (/ *width* aspect))
         (bx 28)
         (by 28)
         (rx (/ (- *width* bx bx) *width*))
         (ry (/ (- height by by) height))
         (surface (create-pdf-surface "out.pdf" *width* height))
         (vals (loop for f in funcs collect
                    (loop for x from (first x-axis) to (second x-axis) by (/ dx samples)
                       collect (list x (funcall f x)))))
         (maxx (loop for i in vals
                  maximizing (apply #'max (loop for x in i collect (first x))) into z finally (return z)))
         (minx (loop for i in vals
                  minimizing (apply #'min (loop for x in i collect (first x))) into z finally (return z)))
         (maxy (loop for i in vals
                  maximizing (apply #'max (loop for x in i collect (second x))) into z finally (return z)))
         (miny (loop for i in vals
                  minimizing (apply #'min (loop for x in i collect (second x))) into z finally (return z))))
    (flet ((tx (x)
             (+ bx (* rx (/ (- x minx) (- maxx minx))
                      *width*)))
           (ty (y)
             (- height (+ by (* ry (/ (- y miny) (- maxy miny))
                      height)))))
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
                (show-text (format nil "~A" x))))

      ;; draw ticks along y axis
      (loop for y in (thin 10 (integers-between miny maxy))
           do (progn
                (move-to (+ (tx minx) 6) (ty y))
                (line-to (- (tx minx) 6) (ty y))
                (stroke)
                (move-to (+ (tx maxx) 6) (ty y))
                (line-to (- (tx maxx) 6) (ty y))
                (stroke)

                (move-to (- (tx minx) 18) (ty y))
                (show-text (format nil "~A" y))))

      ;; draw graph
      (apply #'set-source-rgb foreground)
      (loop for p in vals do
           (loop for (x y) in p
              as flag = t then nil
              do (if flag
                     (move-to (tx x) (ty y))
                     (line-to (tx x) (ty y)))))
      (stroke))
      ;; draw title
    (destroy *context*)))

