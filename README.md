# plot2d
A simple 2-D plot generator for CL. 

## Overview

This project is intended to be an easy-to-use library for generating
2D plots. Plots are produced as pdf, svg, or ps files by way of cairo
(cl-cairo2).

## Samples

Start by creating a generator. There are several generators available depending on whether you are plotting functions of one variable or two, cartesian or polar, etc.

~~~lisp
(ql:quickload :plot2d)
(use-package :plot2d)

(defvar gen (make-instance 'plot2d))
~~~

Once you have a generator, you can use it to create a function plot:

~~~lisp 
(plot gen #'sin)
~~~

# ![ex1](https://raw.github.com/jlowder/plot2d/master/samples/ex1.png)

Or you can pass a list of functions to plot concurrently:

~~~lisp
(plot gen (list #'sin #'cos #'atan))
~~~

# ![ex2](https://raw.github.com/jlowder/plot2d/master/samples/ex2.png)

The plot can be adjusted 

~~~lisp
(plot (list #'sin #'cos #'atan) :x-axis '(-3.14 3.14) :labels '("x-axis" "y-axis") :legend '("sine(x)" "cosine(x)" "atan(x)") :background '(1 1 1) :palette '((1 0 1) (0 1 1) (1 .5 0)))
~~~

# ![ex3](https://raw.github.com/jlowder/plot2d/master/samples/ex3.png)

More complex plots can be made by passing lists of x and y values to plot-xy:

~~~lisp
(let* ((l (loop for x from 0 to 5000 collect x))
        (theta (loop for x in l collect (* 24/5000 x pi)))
        (r (mapcar #'(lambda (x) (+ (- (expt (exp 1) (cos x)) (* 2 (cos (* 4 x)))) (expt (sin (/ x 12)) 5))) theta))
        (x (loop for a in r for b in theta collect (funcall #'(lambda (x y) (* x (cos (+ y (/ pi 2))))) a b)))
        (y (loop for a in r for b in theta collect (funcall #'(lambda (x y) (* x (sin (+ y (/ pi 2))))) a b))))
   (plot/xy x y :background '(1 1 1) :palette (list '(0 0 1)) :legend '("Professor Fey's Butterfly")))
~~~

# ![ex4](https://raw.github.com/jlowder/plot2d/master/samples/ex4.png)

## License

MIT
