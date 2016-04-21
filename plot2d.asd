(defsystem plot2d
    :name "plot2d"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :license "MIT"
    :description "Simple 2D plot library based on cairo"
    :depends-on (:cl-cairo2)
    :components ((:file "plot2d")))
