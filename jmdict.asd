;;;; jmdict.asd --- ASDF system definition for jmdict

;;;; Commentary:

;;; TODO

;;;; Code:

(defsystem :jmdict
  :description "A utility to convert JMDict and related project files to an SQLite database."
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-ppcre" "s-xml" "sqlite" "trivial-types")
  :components ((:file "packages")))

;;;; jmdict.asd ends here
