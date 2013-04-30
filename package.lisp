;;;; package.lisp

(defpackage #:spam-filter
  (:use #:cl)
  (:export classification
           word-feature
           *min-ham-score*
           *max-ham-score*
           *feature-database*
           clear-database
           extract-words
           extract-features
           intern-feature) 
  )


