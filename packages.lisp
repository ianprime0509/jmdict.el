;;;; packages.lisp --- package definition for jmdict

;;; Copyright 2019 Ian Johnson

;;; This file is part of jmdict.el, a free software project for
;;; integrating the JMDict and Kanjidic dictionaries into Emacs. You
;;; are free to use, distribute and change the code in this project
;;; subject to the MIT license included in the project root directory.

;;;; Commentary:

;;; This is where the jmdict package is defined.

;;;; Code:

(in-package :cl-user)

(defpackage :jmdict
  (:documentation "Jmdict is a simple utility for converting JMDict and other Japanese reference files to a unified SQLite database.")
  (:use :cl))

;;;; packages.lisp ends here
