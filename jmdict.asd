;;;; jmdict.asd --- ASDF system definition for jmdict

;;; Copyright 2019 Ian Johnson

;;; This file is part of jmdict.el, a free software project for
;;; integrating the JMDict and Kanjidic dictionaries into Emacs. You
;;; are free to use, distribute and change the code in this project
;;; subject to the MIT license included in the project root directory.

;;;; Commentary:

;;; This is the ASDF system definition for jmdict.

;;;; Code:

(defsystem :jmdict
  :description "A utility to convert JMDict and related project files to an SQLite database."
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre" "s-xml" "sqlite")
  :components ((:file "packages")
               (:file "jmdict" :depends-on ("packages"))))

;;;; jmdict.asd ends here
