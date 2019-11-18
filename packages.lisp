;;;; packages.lisp --- package definition for jmdict

;;;; Commentary:

;;; This is where the jmdict package is defined.

;;;; Code:

(in-package :cl-user)

(defpackage :jmdict
  (:documentation "Jmdict is a simple utility for converting JMDict and other Japanese reference files to a unified SQLite database.")
  (:use :cl :trivial-types))

;;;; packages.lisp ends here
