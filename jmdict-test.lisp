;;;; jmdict-test.lisp --- tests for jmdict.lisp

;;; Copyright 2019 Ian Johnson

;;; This file is part of jmdict.el, a free software project for
;;; integrating the JMDict and Kanjidic dictionaries into Emacs. You
;;; are free to use, distribute and change the code in this project
;;; subject to the MIT license included in the project root directory.

;;;; Commentary:

;;; This file contains some basic tests for the code in jmdict.lisp to
;;; ensure it doesn't break when modifications are made.

;;;; Code:

(in-package :cl-user)

(defpackage :jmdict-test
  (:use :cl :5am))

(in-package :jmdict-test)

(declaim (optimize (speed 0) debug))

;;; Test helpers

(defmacro with-temporary-file (file &body body)
  "Execute BODY with the temporary file described by FILE.
FILE is either a symbol, which will be bound to the path of the
temporary file, or a list of the form (PATH-VAR STRING), where
PATH-VAR will be bound to the path of the file and STRING will be the
file's initial contents."
  (ctypecase file
    (symbol `(uiop:with-temporary-file (:pathname ,file) ,@body))
    (list (destructuring-bind (path-var string) file
            `(with-temporary-file ,path-var
               (overwrite-file-with-string ,path-var ,string)
               ,@body)))))

(defmacro with-temporary-files (files &body body)
  "Execute BODY with the temporary files described in FILES.
Each element of FILES is of the form described in
WITH-TEMPORARY-FILE."
  (if files
      `(with-temporary-file ,(first files)
         (with-temporary-files ,(rest files)
           ,@body))
      `(progn ,@body)))

(defun overwrite-file-with-string (file string)
  "Replace the contents of FILE with STRING."
  (with-open-file (output file :direction :output :if-exists :supersede)
    (write-string string output)))

;;; Tests

(test xml-to-sqlite
  (let ((xml "<!DOCTYPE Test [
<!ENTITY test \"test\">
]>
<root>
<entry>
  <word>one</word>
  <type>&test;</type>
  <metadata kind=\"first\">first</metadata>
  <metadata kind=\"second\">1st</metadata>
  <definition><meaning>the number one</meaning></definition>
</entry>
<entry>
  <word>two</word>
  <metadata kind=\"first\">second</metadata>
  <metadata kind=\"second\">2nd</metadata>
  <definition><meaning>the number two</meaning></definition>
  <definition><meaning>the second thing</meaning></definition>
</entry>
</root>")
        (structure '(:|entry|
                     ("Entry"
                      ("word" "TEXT NOT NULL" (:|word| :text)
                       :indexed t)
                      ("type" "TEXT" (:|type| :text)))
                     (:|metadata|
                      ("Metadata"
                       ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)"
                                   :parent-id
                                   :indexed t)
                       ("metadata" "TEXT NOT NULL" (:text))
                       ("kind" "TEXT NOT NULL" (:|kind|))))
                     ((:|definition| :|meaning|)
                      ("Meaning"
                       ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)"
                                   :parent-id)
                       ("meaning" "TEXT NOT NULL" (:text)))))))
    (with-temporary-files ((xml-path xml)
                           db-path)
      (jmdict:convert-xml-to-sqlite xml-path db-path structure)
      (sqlite:with-open-database (db db-path)
        (is (= 2 (sqlite:execute-single db "SELECT count(*) FROM Entry;")))
        (is (= 4 (sqlite:execute-single db "SELECT count(*) FROM Metadata;")))
        (is (= 3 (sqlite:execute-single db "SELECT count(*) FROM Meaning;")))
        (is (equal '((1 "one" "test") (2 "two" nil))
                   (sqlite:execute-to-list
                    db "SELECT id, word, type FROM Entry;")))
        (is (equal '(("one" "first" "first") ("one" "second" "1st"))
                   (sqlite:execute-to-list
                    db "SELECT word, kind, metadata
FROM Entry e
JOIN Metadata m
ON e.id = m.entry_id
WHERE e.id = 1;")))
        (is (equal '(("the number two") ("the second thing"))
                   (sqlite:execute-to-list
                    db "SELECT meaning
FROM Entry e
JOIN Meaning m
ON e.id = m.entry_id
WHERE e.id = 2;")))))))

;;;; jmdict-test.lisp ends here
