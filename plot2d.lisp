(ql:quickload :cl-cairo2)
(use-package :cairo)

(defparameter *context* nil)
(defparameter *width* 800)

(defun integers-between (x y)
  (loop for i from (ceiling x) to (floor y) collect i))

(defun half-ints-between (x y)
  (remove-if #'(lambda (n) (or (< n x) (> n y))) 
             (loop for i from (floor x) to (ceiling y) by 0.5 collect i)))

(defun strcat (l &optional (ret nil))
  (if l
      (strcat (cdr l) (concatenate 'string ret (car l) (string #\Newline)))
      (string-trim '(#\Newline) ret)))

(defun thin (n l)
  (loop for i from 0 to (1- (length l)) by (ceiling (/ (length l) n)) collect (nth i l)))

(defun plot2d (funcs &key (x-axis (list -2 2)) (aspect 1.0) (samples 100) (background (list 0.2 0.2 0.2)) (palette '((1.0 0.2 0.2) (0.2 1.0 0.2) (0.2 0.2 1.0)))
                       (legend nil))
  (let* ((funcs (if (listp funcs) funcs (list funcs)))
         (colors (loop for x in funcs appending (loop for y in palette collect y)))
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
      (when (<= (length (half-ints-between miny maxy)) 20)
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
        (let ((bg (append (mapcar #'(lambda (x) (- 1.0 x)) background) '(0.8))))
          (destructuring-bind (xb yb w h) (max-extents legend)
            (apply #'set-source-rgba bg)
            (rectangle (+ 55 xb) (+ yb 55) (+ w 75) (+ 10 (* h (length legend))))
            (fill-path)
            (loop for txt in legend
               for c in colors
               as y = 60 then (+ y h) do
                 (apply #'set-source-rgb background)
                 (move-to 60 y)
                 (show-text txt)
                 (move-to (+ 65 w xb) (+ (/ yb 2) y))
                 (apply #'set-source-rgb c)
                 (line-to (+ 125 w xb) (+ (/ yb 2) y))
                 (stroke))))))
            
    (destroy *context*)))

