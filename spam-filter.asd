;;;; spam-filter.asd

(asdf:defsystem #:spam-filter
  :serial t
  :description "spam and ham filter from practical common lisp"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre
               #:logan-pathnames)
  :components ((:file "package")
               (:file "spam-filter")))

