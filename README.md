# plot2d
A simple 2-D plot generator for CL. 

## Overview

This project is intended to be an easy-to-use library for generating
2D plots. Plots are produced as pdf, svg, or ps files by way of cairo
(cl-cairo2).

## Samples

Start by creating a generator. There are several generators available
depending on whether you are plotting functions of one variable or
two, cartesian or polar, etc.

~~~lisp
(ql:quickload :plot2d)
(use-package :plot2d)

(defvar gen (make-instance 'plot2d))
~~~

Once you have a generator you can use it to create a function plot:

~~~lisp 
(plot gen #'sin)
~~~

# ![ex1](https://raw.github.com/jlowder/plot2d/master/samples/ex1.png)

You can pass in a list of functions instead of a single function. A
legend and axis labels can be added via keyword arguments.

~~~lisp
(plot gen (list #'sin #'cos #'atan) :legend '("sine" "cosine" "atan") :placement '(30 30) :labels '("X-Axis" "Y-Axis"))
~~~

# ![ex2](https://raw.github.com/jlowder/plot2d/master/samples/ex2.png)

More complex plots can be made by passing lists of precomputed x and y values to plot/xy:

~~~lisp
(let* ((theta (loop for x from 0 to 5000 collect (* 24/5000 x pi)))
       (r (mapcar #'(lambda (x) (+ (- (expt (exp 1) (cos x)) (* 2 (cos (* 4 x)))) (expt (sin (/ x 12)) 5))) theta))
       (x (loop for a in r for b in theta collect (funcall #'(lambda (x y) (* x (cos (+ y (/ pi 2))))) a b))) ; transform to cartesian and rotate
       (y (loop for a in r for b in theta collect (funcall #'(lambda (x y) (* x (sin (+ y (/ pi 2))))) a b))))
   (plot/xy gen x y :labels '("Professor Fey's Butterfly" "")))
~~~

# ![ex3](https://raw.github.com/jlowder/plot2d/master/samples/ex3.png)

This example is from the book "A First Course in Computational
Physics" by Paul L. DeVries. Many other plots from this book are generated
by the "classic-curves.ros" script in the _samples_ folder.

# ![montage](https://raw.github.com/jlowder/plot2d/master/samples/montage.png)

## API

Coming soon -- see the _samples_ folder for now.

## License

MIT
