;;;; jmdict.lisp --- parsing and conversion code for JMDict and Kanjidic

;;; Copyright 2019 Ian Johnson

;;; This file is part of jmdict.el, a free software project for
;;; integrating the JMDict and Kanjidic dictionaries into Emacs. You
;;; are free to use, distribute and change the code in this project
;;; subject to the MIT license included in the project root directory.

;;;; Commentary:

;;; This file contains functions for converting the JMDict and
;;; Kanjidic XML files to SQLite databases, which is a more convenient
;;; format for quick access (much faster than parsing XML on the fly
;;; for every query).

;;;; Code:

(in-package :jmdict)

;; This is an unfortunate workaround for the fact that s-xml uses
;; symbols for tag names and packages to represent the XML namespace
;; of a tag
(intern "lang" :xml)
(export (find-symbol "lang" :xml) :xml)

(defparameter *jmdict-structure*
  '(:|entry|
    ("Entry"
     ("sequence_number" "INTEGER NOT NULL UNIQUE" (:|ent_seq| :text)))
    (:|k_ele|
     ("Kanji"
      ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)" :parent-id)
      ("reading" "TEXT NOT NULL" (:|keb| :text)
                 :comment "Reading in kanji or other non-kana characters")))
    (:|r_ele|
     ("Reading"
      ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)" :parent-id)
      ("reading" "TEXT NOT NULL" (:|reb| :text)
                 :comment "Reading in kana"))
     (:|re_restr|
      ("ReadingRestriction"
       ("reading_id" "INTEGER NOT NULL REFERENCES Reading(id)" :parent-id)
       ("restriction" "TEXT NOT NULL" (:text)))))
    (:|sense|
     ("Sense"
      ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)" :parent-id))
     (:|xref|
      ("SenseCrossReference"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id)
       ("target" "TEXT NOT NULL" (:text)
                 :comment "A word in kanji or kana")))
     (:|ant|
      ("SenseAntonym"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id)
       ("target" "TEXT NOT NULL" (:text)
                 :comment "A word in kanji or kana")))
     (:|pos|
      ("SensePartOfSpeech"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id)
       ("part_of_speech" "TEXT NOT NULL" (:text))))
     (:|gloss|
      ("Gloss"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id)
       ;; Note: the JMDict entry for 畜生 actually has a gloss element
       ;; with no contents (which, interestingly, is in Spanish).
       ;; Rather than try to figure out a way to accommodate this in
       ;; the code somehow, for now I've just removed the obvious NOT
       ;; NULL constraint.
       ("gloss" "TEXT" (:text))
       ("language" "TEXT NOT NULL" (:or (xml:|lang|) "eng")
                   :comment "The three-letter language code of the gloss")
       ("gender" "TEXT" (:|g_gend|)
                 :comment "The gender of the gloss")
       ("type" "TEXT" (:|g_type|)
               :comment "The type (e.g. literal, figurative) of the gloss")))))
  "The structure of the JMDict database.
The structure is represented as an alist of recursive 'trees' of
elements, mimicking the schema of the original XML file. Each tree is
a list, with the first element (the 'root') being a table description
and the remaining elements being alists of structure trees
corresponding to sub-elements in the XML file.

Each table description is a list of the form (NAME &rest COLUMNS),
where each column is itself a list of the form (NAME TYPE PATH &key
:COMMENT). TYPE is a SQLite data type, including column constraints.
PATH is a path (interpreted by STRUCTURE-PATH) to the data stored in
the column. COMMENT is, optionally, a comment to include in the table
schema (documentation).

Following the table description, the rest of the structure is an alist
of sub-structures, where the keys are sub-elements in the XML file.
Each value in the alist is a structure of the same form as this one
and will be processed in the same way for the corresponding
sub-element.

Note: only one top-level element is supported (or needed).")

(defparameter *kanjidic-structure*
  '(:|character|
    ("Character"
     ("literal" "TEXT NOT NULL" (:|literal| :text))
     ("grade" "INTEGER" (:|misc| :|grade| :text))
     ("stroke_count" "INTEGER" (:|misc| :|stroke_count| :text))
     ("jlpt_level" "INTEGER" (:|misc| :|jlpt| :text)))
    (:|reading_meaning|
     ("ReadingMeaning"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id))
     (:|rmgroup|
      ("ReadingMeaningGroup"
       ("reading_meaning_id" "INTEGER NOT NULL REFERENCES ReadingMeaning(id)" :parent-id))
      (:|reading|
        ("Reading"
         ("reading_meaning_group_id" "INTEGER NOT NULL REFERENCES ReadingMeaningGroup(id)" :parent-id)
         ("reading" "TEXT NOT NULL" (:text))
         ("type" "TEXT NOT NULL" (:|r_type|))
         ("on_type" "TEXT" (:|on_type|))
         ("approved" "TEXT" (:|r_status|))))
      (:|meaning|
        ("Meaning"
         ("reading_meaning_group_id" "INTEGER NOT NULL REFERENCES ReadingMeaningGroup(id)" :parent-id)
         ("meaning" "TEXT NOT NULL" (:text))
         ("language" "TEXT NOT NULL" (:or (:|m_lang|) "en")
                     :comment "The two-letter language code of the meaning"))))
     (:|nanori|
      ("Nanori"
       ("reading_meaning_id" "INTEGER NOT NULL REFERENCES ReadingMeaning(id)" :parent-id)
       ("nanori" "TEXT NOT NULL" (:text))))))
  "The structure of the Kanjidic XML file in the same format as *JMDICT-STRUCTURE*.")

(defun convert-xml-to-sqlite (xml-path sqlite-path structure)
  "Convert an XML file to a SQLite database following STRUCTURE.
STRUCTURE is a database structure in the format of
*JMDICT-STRUCTURE*."
  (sqlite:with-open-database (db sqlite-path)
    (create-tables db structure)
    (insert-values db xml-path structure)))

;;; SQLite interaction

(defun create-tables (db structure)
  "Create the tables specified by STRUCTURE in DB, dropping them if they already exist."
  (labels ((format-column-string (stream column &optional last)
             (destructuring-bind (name type path &key comment) column
               (declare (ignore path))
               (format stream "~a ~a" name type)
               (unless last (format stream ","))
               (when comment (format stream " -- ~a" comment))
               (format stream "~%")))
           (format-table-string (stream table)
             (destructuring-bind (name &rest columns) table
               (format stream "CREATE TABLE ~a (~%" name)
               ;; Every table has a primary key column
               (format-column-string stream '("id" "INTEGER PRIMARY KEY" nil)
                                     (endp columns))
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
    (destructuring-bind (element table &rest sub-structures) structure
      (declare (ignore element))
      (drop-table-if-exists table)
      (create-table table)
      (loop for sub-structure in sub-structures
         do (create-tables db sub-structure)))))

(defun insert-values (db xml-path structure)
  "Insert the data from XML-PATH into DB following STRUCTURE."
  (with-open-file (input xml-path)
    ;; The process below relies on the fact that PARSE-XML-ENTITIES
    ;; will advance the stream to the first line after the DOCTYPE,
    ;; meaning that s-xml doesn't have to attempt to parse the
    ;; DOCTYPE. This is unfortunately necessary, since s-xml chokes on
    ;; the Kanjidic DOCTYPE (it gets confused with single quotes in
    ;; comments, I think)
    (let ((entities (parse-xml-entities input)))
      (sqlite:with-transaction db
        (let ((id-counters (make-hash-table))
              (prepared-statements (make-hash-table))
              (n 0))
          (parse-xml-entries input (first structure)
                             (lambda (entry)
                               (insert-value db entry (rest structure)
                                             id-counters prepared-statements)
                               (when (zerop (rem (incf n) 10000))
                                 (format *error-output*
                                         "Processed ~d entries" n)))
                             :entities entities)))))

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
            (column-values (mapcar (lambda (column)
                                     (let ((path (third column)))
                                       (case path
                                         (:parent-id parent-id)
                                         (t (structure-path path value)))))
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
                                  :parent-id id)))))))

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
