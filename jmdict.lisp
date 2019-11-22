;;;; jmdict.lisp --- parsing and conversion code for JMDict and Kanjidic

;;;; Commentary:

;;; TODO

;;;; Code:

(defparameter *jmdict-tables*
  '(("Entry"
     ("id" "INTEGER PRIMARY KEY")
     ("sequence_number" "INTEGER NOT NULL UNIQUE"))
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
     ;; Note: the JMDict entry for 畜生 actually has a gloss element
     ;; with no contents (which, interestingly, is in Spanish). Rather
     ;; than try to figure out a way to accommodate this in the code
     ;; somehow, for now I've just removed the obvious NOT NULL
     ;; constraint.
     ("gloss" "TEXT"
      :comment "A target-language word or phrase translating the Japanese word")
     ("language" "TEXT NOT NULL"
      :comment "The three-letter language code of the gloss")
     ("gender" "TEXT"
      :comment "The gender of the gloss")
     ("type" "TEXT"
      :comment "The type (e.g. literal, figurative) of the gloss")))
  "A list of the tables to create for the JMDict database.")

(defparameter *jmdict-structure*
  '(("Entry" (:|ent_seq| :text))
    (:|k_ele|
     ("Kanji" :parent-id (:|keb| :text)))
    (:|r_ele|
     ("Reading" :parent-id (:|reb| :text))
     (:|re_restr|
      ("ReadingRestriction" :parent-id (:text))))
    (:|sense|
     ("Sense" :parent-id)
     (:|xref|
      ("SenseCrossReference" :parent-id (:text)))
     (:|ant|
      ("SenseAntonym" :parent-id (:text)))
     (:|pos|
      ("SensePartOfSpeech" :parent-id (:text)))
     (:|gloss|
      ("Gloss" :parent-id (:text) (:or (xml:|lang|) "eng")
               (:|g_gend|) (:|g_type|))))))

(defun convert-jmdict-to-sqlite (jmdict-path sqlite-path)
  "Convert the JMDict XML file at JMDICT-PATH to a SQLite database at SQLITE-PATH."
  (sqlite:with-open-database (db sqlite-path)
    (create-tables db *jmdict-tables*)
    (insert-jmdict db jmdict-path)))

;;; SQLite interaction

(defun create-tables (db tables)
  "Create the tables specified by TABLES in DB, dropping them if they already exist.
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
               (sqlite:execute-non-query db create-statement)))
           (drop-table-if-exists (table)
             (let ((drop-statement
                    (format nil "DROP TABLE IF EXISTS ~a" (first table))))
               (sqlite:execute-non-query db drop-statement))))
    (loop for table in tables
       do (drop-table-if-exists table)
         (create-table table))))

(defun insert-jmdict (db jmdict-path)
  "Insert the JMDict data from the file at JMDICT-PATH into DB."
  (sqlite:with-transaction db
    (let ((entities (with-open-file (input jmdict-path)
                      (parse-xml-entities input))))
      (with-open-file (input jmdict-path)
        (let ((id-counters (make-hash-table))
              (prepared-statements (make-hash-table))
              (n 0))
          (parse-xml-entries input :|entry|
                             (lambda (entry)
                               (insert-value db entry *jmdict-structure*
                                             id-counters prepared-statements)
                               (when (zerop (rem (incf n) 10000))
                                 (print n)))
                             :entities entities))))))

(defun insert-value (db value structure id-counters
                     prepared-statements &key parent-id)
  "Insert VALUE into DB according to STRUCTURE.
STRUCTURE is a list of table definitions like *JMDICT-STRUCTURE* that
defines how to traverse VALUE and insert its components into the
correct tables.

ID-COUNTERS is a hash table maintaining a unique ID for each row
inserted into each table. PREPARED-STATEMENTS is a hash table
maintaining statements prepared for inserting rows into each table.

PARENT-ID is the unique ID of the 'parent' element of this value (the
enclosing XML element), if there is such a parent."
  (destructuring-bind (table &rest columns) (first structure)
    (let ((statement
           (gethash-lazy table prepared-statements
                         (prepare-non-query-statement
                          db
                          (make-insert-query table (1+ (length columns))))))
          (column-values (mapcar (lambda (path)
                                   (case path
                                     (:parent-id parent-id)
                                     (t (structure-path path value))))
                                 columns))
          (id (incf (gethash table id-counters 0))))
      (apply statement id column-values)
      ;; Process sub-structures
      (loop for (subelement . substructure) in (rest structure)
         do (loop for child-value in (cdr (assoc subelement value))
               ;; The table structure definition is recursive: we can
               ;; continue in the same fashion for all child values
               do (insert-value db child-value substructure
                                id-counters prepared-statements
                                :parent-id id))))))

(defun make-insert-query (table n-columns)
  "Create an insert query for TABLE with N-COLUMNS columns."
  (with-output-to-string (query)
    (format query "INSERT INTO ~a VALUES (" table)
    (loop repeat (1- n-columns) do (format query "?, "))
    (format query "?)")))

(defun prepare-non-query-statement (db sql)
  "Prepare a non-query statement that can be called as a function binding its parameters to the function argument."
  (let ((statement (sqlite:prepare-statement db sql)))
    (lambda (&rest params)
      (loop for param in params
         for i from 1
         do (sqlite:bind-parameter statement i param))
      (sqlite:step-statement statement)
      (sqlite:reset-statement statement))))

(defmacro with-prepared-statements (statements db &body body)
  "Execute BODY with functions created for the prepared statements in STATEMENTS.
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
                            (sqlite:step-statement ,sym)
                            (sqlite:reset-statement ,sym))
                functions))
     finally (return
               `(let ,bindings
                  (labels ,functions
                    (unwind-protect (progn ,@body)
                      ,@(loop for sym in syms
                           collect `(sqlite:finalize-statement ,sym))))))))

;;; XML parsing

(defun structure-path (path structure)
  "Return the result of following PATH in STRUCTURE.
PATH is a list of tag names: each tag will be followed in sequence and
the first element matching the tag extracted. For example, the
path (:k_ele :keb :TEXT) would return the first kanji reading of an
element. If the path cannot be followed completely, return NIL.

As a special case, if PATH is of the form (:OR PATH VALUE), then VALUE
will be returned instead of the value at PATH if the same is NIL."
  (if (eql :or (first path))
      (or (structure-path (second path) structure) (third path))
      (loop with result = structure
         for tag in path
         do (setf result (cadr (assoc tag result)))
         finally (return result))))

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
as sub-elements, treating the attribute name as the tag name. All text
in the element is treated as a special 'TEXT' element.

Note that this notion of 'structure' does not preserve the usual
structure of XML; it is just good enough to process 'structured' data
formats like JMDict and Kanjidic.

ENTITIES is a hashtable giving the expansions of XML entities."
    (labels ((add-element (alist key value)
               (let ((assoc (assoc key alist)))
                 (if assoc
                     (prog1 alist
                       (setf (cdr assoc) (nconc (cdr assoc) (list value))))
                     (cons (list key value) alist))))
             (convert-attributes (attributes)
               (mapcar (lambda (attr)
                         (cons (car attr) (list (cdr attr))))
                       attributes))
             (new-element-hook (name attributes seed)
               (declare (ignore name attributes seed))
               ())
             (finish-element-hook (name attributes parent-seed seed)
               (let ((structure (append (convert-attributes attributes)
                                        seed)))
                 (if (eql name element)
                     ;; If we're at the target element, all we need to
                     ;; do is call our handler
                     (funcall handler structure)
                     ;; Otherwise, we need to append this element's
                     ;; structure to the parent's list of elements
                     ;; with this name
                     (add-element parent-seed name structure))))
             (text-hook (string seed)
               (add-element seed :text string)))
      (s-xml:start-parse-xml
       input
       (make-instance 's-xml:xml-parser-state
                      :entities (or entities (s-xml::make-standard-entities))
                      :seed ()
                      :new-element-hook #'new-element-hook
                      :finish-element-hook #'finish-element-hook
                      :text-hook #'text-hook)))
    nil)

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

;;; Utility functions

(defmacro gethash-lazy (key table default)
  "Return the value of KEY in the hash table TABLE with a (lazily computed) default value DEFAULT."
  (let ((value (gensym))
        (present-p (gensym)))
    `(multiple-value-bind (,value ,present-p) (gethash ,key ,table)
       (if ,present-p
           (values ,value t)
           (let ((,value ,default))
             (setf (gethash ,key ,table) ,value)
             (values ,value t))))))

;;;; jmdict.lisp ends here
