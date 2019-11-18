;;;; jmdict.lisp --- parsing and conversion code for JMDict and Kanjidic

;;;; Commentary:

;;; TODO

;;;; Code:

(defparameter *jmdict-tables*
  '(("Entry"
     ("id" "INTEGER PRIMARY KEY"))
    ("Kanji"
     ("id" "INTEGER PRIMARY KEY")
     ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)")
     ("reading" "TEXT NOT NULL"
      :comment "Reading in kanji or other non-kana characters"))
    ("Reading"
     ("id" "INTEGER PRIMARY KEY")
     ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)")
     ("reading" "TEXT NOT NULL"
      :comment "Reading in kana"))
    ("ReadingRestriction"
     ("id" "INTEGER PRIMARY KEY")
     ("reading_id" "INTEGER NOT NULL REFERENCES Reading(id)")
     ("restriction" "TEXT NOT NULL"))
    ("Sense"
     ("id" "INTEGER PRIMARY KEY")
     ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)"))
    ("SenseCrossReference"
     ("id" "INTEGER PRIMARY KEY")
     ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)")
     ("target" "TEXT NOT NULL"
      :comment "A word in kanji or kana"))
    ("SenseAntonym"
     ("id" "INTEGER PRIMARY KEY")
     ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)")
     ("target" "TEXT NOT NULL"
      :comment "A word in kanji or kana"))
    ("SensePartOfSpeech"
     ("id" "INTEGER PRIMARY KEY")
     ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)")
     ("part_of_speech" "TEXT NOT NULL"))
    ("SenseField"
     ("id" "INTEGER PRIMARY KEY")
     ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)")
     ("field" "TEXT NOT NULL"
      :comment "The field of application of the entry/sense"))
    ("Gloss"
     ("id" "INTEGER PRIMARY KEY")
     ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)")
     ("gloss" "TEXT NOT NULL"
      :comment "A target-language word or phrase translating the Japanese word")
     ("language" "TEXT NOT NULL"
      :comment "The three-letter language code of the gloss")
     ("gender" "TEXT"
      :comment "The gender of the gloss")
     ("type" "TEXT"
      :comment "The type (e.g. literal, figurative) of the gloss")))
  "A list of the tables to create for the JMDict database.")

(defun convert-jmdict-to-sqlite (xml-path sqlite-path)
  "Convert the JMDict XML file at XML-PATH to a SQLite database at SQLITE-PATH."
  (sqlite:with-open-database (db sqlite-path)
    (create-tables db *jmdict-tables*)))

;;; SQLite interaction

(defun create-tables (db tables)
  "Create the tables specified by TABLES in DB.
TABLES is an alist of table names to columns in the table. Each column
is a list of the form (NAME TYPE &key COMMENT)."
  (labels ((format-column-string (stream column &optional last)
             (destructuring-bind (name type &key comment) column
               (format stream "~a ~a" name type)
               (unless last (format stream ","))
               (when comment (format stream " -- ~a" comment))
               (format stream "~%")))
           (format-table-string (stream table)
             (destructuring-bind (name &rest columns) table
               (format stream "CREATE TABLE ~a (~%" name)
               (loop for rest on columns
                  do (format-column-string stream (car rest) (endp (cdr rest))))
               (format stream ");")))
           (create-table (table)
             (let ((create-statement
                    (with-output-to-string (statement)
                      (format-table-string statement table))))
               (sqlite:execute-non-query db create-statement))))
    (loop for table in tables do (create-table table))))

(defun insert-jmdict (db jmdict-path)
  "Insert the JMDict data from the file at JMDICT-PATH into DB."
  (let ((entities (parse-xml-entities jmdict-path)))
    (with-prepared-statements
        ((insert-entry "INSERT INTO Entry VALUES (?)")
         (insert-kanji "INSERT INTO Kanji VALUES (?, ?, ?)")
         (insert-reading "INSERT INTO Reading VALUES (?, ?, ?)")) db
      (flet ((process-entry (entry)
               ))))))

(defmacro with-prepared-statements (statements db &body body)
  "Executes BODY with functions created for the prepared statements in STATEMENTS.
STATEMENTS is a list of lists of the form (FUNCTION STATEMENT), where
FUNCTION is the name of a function to be defined in BODY that calls
the prepared statement STATEMENT with the parameters passed to the
function. This is really only useful for non-query statements, since
the prepared statement is only stepped once."
  (loop with syms and bindings and functions
     for (function statement) in statements
     do (let ((sym (gensym)))
          (push sym syms)
          (push `(,sym (sqlite:prepare-statement ,db ,statement)) bindings)
          (push `(,function (&rest params)
                            (loop for param in params
                               for i from 1
                               do (sqlite:bind-parameter ,sym i param))
                            (sqlite:step-statement ,sym))
                functions))
     finally (return
               `(let ,bindings
                  (labels ,functions
                    (unwind-protect (progn ,@body)
                      ,@(loop for sym in syms
                           collect `(sqlite:finalize-statement ,sym))))))))

;;; XML parsing

(defun parse-dom-elements (input element handler &key entities)
  "Parse a stream of XML from INPUT and call HANDLER with the DOM of every top-level ELEMENT.
This is useful for parsing an XML file structured as a large number of
repeating elements, which covers both cases we're interested in for
this project (JMDict and Kanjidic). The DOM is passed in the
XML-STRUCT format of s-xml.

ENTITIES is a hashtable giving the expansions of XML entities."
  ;; In the hooks below, the seed is a list of the elements
  ;; encountered so far (in reverse order)
  (labels ((new-element-hook (name attributes seed)
             (declare (ignore name attributes seed)))
           (finish-element-hook (name attributes parent-seed seed)
             (let ((dom (s-xml:make-xml-element :name name
                                                :attributes attributes
                                                :children (nreverse seed))))
               (if (equal name element)
                   (progn (funcall handler dom)
                          ;; Make sure we return nil no matter what
                          ;; handler returns
                          nil)
                   (cons dom parent-seed))))
           (text-hook (string seed)
             (cons string seed)))
    (s-xml:start-parse-xml
     input
     (make-instance 's-xml:xml-parser-state
                    :entities (or entities (s-xml::make-standard-entities))
                    :seed nil
                    :new-element-hook #'new-element-hook
                    :finish-element-hook #'finish-element-hook
                    :text-hook #'text-hook))))

(defun parse-xml-entries (input element handler &key entities)
    "Parse a stream of XML from INPUT and call HANDLER with the structure of every top-level ELEMENT.
The structure of an XML element is an alist where the keys are element
names and the values are lists of the structures of all sub-elements
with the corresponding name. Attribute values are treated the same way
as sub-elements except the value is not a list, it is just the value
of the attribute. All text in the element is treated as a special
'TEXT' element.

Note that this notion of 'structure' does not preserve the usual
structure of XML, and is not general enough to process all documents;
it is just good enough to process 'structured' data formats like
JMDict and Kanjidic.

ENTITIES is a hashtable giving the expansions of XML entities."
    (labels ((new-element-hook (name attributes seed)
               (declare (ignore name attributes seed))
               ())
             (finish-element-hook (name attributes parent-seed seed)
               (let ((structure (append attributes seed)))
                 (when (eql name element)
                   (funcall handler seed)
                   (return-from finish-element-hook))
                 (let ((assoc (assoc name parent-seed)))
                   (if assoc
                       (setf (cdr assoc) (nconc (cdr assoc) (list structure)))
                       (push (list name seed) parent-seed))))
               parent-seed)
             (text-hook (string seed)
               (let ((assoc (assoc :text seed)))
                 (if assoc
                     (setf (cdr assoc) (nconc (cdr assoc) (list string)))
                     (push (list :text string) seed)))
               seed))
      (s-xml:start-parse-xml
       input
       (make-instance 's-xml:xml-parser-state
                      :entities (or entities (s-xml::make-standard-entities))
                      :seed ()
                      :new-element-hook #'new-element-hook
                      :finish-element-hook #'finish-element-hook
                      :text-hook #'text-hook))))

(defun parse-xml-entities (input)
  "Parse a stream of XML from INPUT and return a hashtable with all entities found in the DOCTYPE (as well as all standard XML entities)."
  ;; Skip input until we reach the DOCTYPE
  (loop with scanner = (ppcre:create-scanner "<!DOCTYPE")
     for line = (read-line input nil)
     until (ppcre:scan scanner line))
  ;; Try to read all the entities we can until we reach ]>
  ;; Unfortunately, make-standard-entities is not exported in s-xml
  (loop with entities = (s-xml::make-standard-entities)
     and entity-scanner = (ppcre:create-scanner
                               "<!ENTITY +([^ ]+) +\"([^\"]+)\">")
     and end-scanner = (ppcre:create-scanner "]>")
     for line = (read-line input nil)
     do (ppcre:register-groups-bind (entity expansion) (entity-scanner line)
          (setf (gethash entity entities) expansion))
     when (ppcre:scan end-scanner line)
     return entities))

;;;; jmdict.lisp ends here
