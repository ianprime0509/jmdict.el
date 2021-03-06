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

(in-package :cl-user)

(defpackage :jmdict
  (:documentation "Jmdict is a simple utility for converting JMDict and other Japanese reference files to a unified SQLite database.")
  (:use :cl)
  (:export
   *jmdict-structure*
   *kanjidic-structure*
   convert-xml-to-sqlite
   convert-tatoeba-to-sqlite))

(in-package :jmdict)

;; This is an unfortunate workaround for the fact that s-xml uses
;; symbols for tag names and packages to represent the XML namespace
;; of a tag
(eval-when (:compile-toplevel :load-toplevel)
  (intern "lang" :xml)
  (export (find-symbol "lang" :xml) :xml))

(defparameter *jmdict-structure*
  '(:|entry|
    ("Entry"
     ("sequence_number" "INTEGER NOT NULL UNIQUE" (:|ent_seq| :text)))
    (:|k_ele|
     ("Kanji"
      ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)" :parent-id
                  :indexed t)
      ("reading" "TEXT NOT NULL COLLATE NOCASE" (:|keb| :text)
                 :comment "Reading in kanji or other non-kana characters"
                 :indexed t))
     (:|ke_inf|
      ("KanjiInfo"
       ("kanji_id" "INTEGER NOT NULL REFERENCES Kanji(id)" :parent-id
                   :indexed t)
       ("info" "TEXT NOT NULL" (:text)
               :comment "Information about kanji orthography")))
     (:|ke_pri|
      ("KanjiPriority"
       ("kanji_id" "INTEGER NOT NULL REFERENCES Kanji(id)" :parent-id
                   :indexed t)
       ("priority" "TEXT NOT NULL" (:text)
                   :comment "Indicator of relative kanji priority"))))
    (:|r_ele|
     ("Reading"
      ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)" :parent-id
                  :indexed t)
      ("reading" "TEXT NOT NULL COLLATE NOCASE" (:|reb| :text)
                 :comment "Reading in kana"
                 :indexed t)
      ("re_nokanji" "INTEGER NOT NULL" (:if (:|re_nokanji|) 1 0)
                    :comment "Indicates that this reading is not a true reading of the kanji"))
     (:|re_restr|
      ("ReadingRestriction"
       ("reading_id" "INTEGER NOT NULL REFERENCES Reading(id)" :parent-id
                     :indexed t)
       ("restriction" "TEXT NOT NULL" (:text))))
     (:|re_inf|
      ("ReadingInfo"
       ("reading_id" "INTEGER NOT NULL REFERENCES Reading(id)" :parent-id
                     :indexed t)
       ("info" "TEXT NOT NULL" (:text)
               :comment "Information about a specific reading")))
     (:|re_pri|
      ("ReadingPriority"
       ("reading_id" "INTEGER NOT NULL REFERENCES Reading(id)" :parent-id
                     :comment "Indicator of relative reading priority")
       ("priority" "TEXT NOT NULL" (:text)))))
    (:|sense|
     ("Sense"
      ("entry_id" "INTEGER NOT NULL REFERENCES Entry(id)" :parent-id
                  :indexed t))
     (:|stagk|
      ("SenseKanjiRestriction"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("restriction" "TEXT NOT NULL" (:text))))
     (:|stagr|
      ("SenseKanaRestriction"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("restriction" "TEXT NOT NULL" (:text))))
     (:|xref|
      ("SenseCrossReference"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("target" "TEXT NOT NULL" (:text)
                 :comment "A word in kanji or kana")))
     (:|ant|
      ("SenseAntonym"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("target" "TEXT NOT NULL" (:text)
                 :comment "A word in kanji or kana")))
     (:|pos|
      ("SensePartOfSpeech"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("part_of_speech" "TEXT NOT NULL" (:text))))
     (:|field|
      ("SenseField"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("field" "TEXT NOT NULL" (:text)
                :comment "The field of application of the sense")))
     (:|misc|
      ("SenseMisc"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("info" "TEXT NOT NULL" (:text)
               :comment "Additional information about the sense")))
     (:|lsource|
      ("SenseSource"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("word" "TEXT" (:text)
               :comment "Word or phrase in source language")
       ("language" "TEXT NOT NULL" (:or (xml:|lang|) "eng"))
       ("type" "TEXT NOT NULL" (:or (:|ls_type|) "full")
               :comment "Either 'full' or 'part' depending on whether this source description fully describes the source of the loan")
       ("wasei" "TEXT NOT NULL" (:or (:|ls_wasei|) "n"))))
     (:|dial|
      ("SenseDialect"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("dialect" "TEXT NOT NULL" (:text)
                  :comment "Dialect code for this sense")))
     (:|gloss|
      ("Gloss"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ;; Note: the JMDict entry for 畜生 actually has a gloss element
       ;; with no contents (which, interestingly, is in Spanish)
       ("gloss" "TEXT NOT NULL COLLATE NOCASE" (:text)
                :indexed t
                :constraints (:not-null))
       ("language" "TEXT NOT NULL" (:or (xml:|lang|) "eng")
                   :comment "The three-letter language code of the gloss"
                   :indexed t)
       ("gender" "TEXT" (:|g_gend|)
                 :comment "The gender of the gloss")
       ("type" "TEXT" (:|g_type|)
               :comment "The type (e.g. literal, figurative) of the gloss")))
     (:|s_inf|
      ("SenseInfo"
       ("sense_id" "INTEGER NOT NULL REFERENCES Sense(id)" :parent-id
                   :indexed t)
       ("info" "TEXT NOT NULL" (:text)
               :comment "Additional information about the sense")))))
  "The structure of the JMDict database.
The structure is represented as an alist of recursive 'trees' of
elements, mimicking the schema of the original XML file. Each tree is
a list, with the first element (the 'root') being a table description
and the remaining elements being alists of structure trees
corresponding to sub-elements in the XML file.

Each table description is a list of the form (NAME &rest COLUMNS),
where each column is itself a list of the form (NAME TYPE PATH &key
:COMMENT :INDEXED). TYPE is a SQLite data type, including column
constraints. PATH is a path (interpreted by STRUCTURE-PATH) to the
data stored in the column. COMMENT is, optionally, a comment to
include in the table schema (documentation). INDEXED, if provided with
a non-NIL value, indicates that an index should be created for the
column.

Following the table description, the rest of the structure is an alist
of sub-structures, where the keys are sub-elements in the XML file.
Each value in the alist is a structure of the same form as this one
and will be processed in the same way for the corresponding
sub-element.

Note: only one top-level element is supported (or needed).")

(defparameter *kanjidic-structure*
  '(:|character|
    ("Character"
     ("literal" "TEXT NOT NULL COLLATE NOCASE" (:|literal| :text)
      :indexed t)
     ("grade" "INTEGER" (:|misc| :|grade| :text))
     ("stroke_count" "INTEGER" (:|misc| :|stroke_count| :text))
     ("frequency" "INTEGER" (:|misc| :|freq| :text)
      :comment "Relative kanji frequency, from 1 (most frequent) to 2500")
     ("jlpt_level" "INTEGER" (:|misc| :|jlpt| :text)))
    ((:|codepoint| :|cp_value|)
     ("Codepoint"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t)
      ("codepoint" "TEXT NOT NULL" (:text))
      ("type" "TEXT NOT NULL" (:|cp_type|))))
    ((:|radical| :|rad_value|)
     ("Radical"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t)
      ("radical" "INTEGER NOT NULL" (:text))
      ("type" "TEXT NOT NULL" (:|rad_type|))))
    ((:|misc| :|variant|)
     ("Variant"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t)
      ("variant" "TEXT NOT NULL" (:text))
      ("type" "TEXT NOT NULL" (:|var_type|))))
    ((:|dic_number| :|dic_ref|)
     ("DictionaryReference"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t)
      ("reference" "INTEGER NOT NULL" (:text))
      ("type" "TEXT NOT NULL" (:|dr_type|))
      ("volume" "INTEGER" (:|m_vol|)
                :comment "Volume number for Daikanwajiten (moro)")
      ("page" "INTEGER" (:|m_page|)
              :comment "Page number for Daikanwajiten (moro)")))
    ((:|query_code| :|q_code|)
     ("QueryCode"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t)
      ("code" "TEXT NOT NULL" (:text))
      ("type" "TEXT NOT NULL" (:|qc_type|))
      ("skip_misclass" "TEXT" (:|skip_misclass|))))
    ((:|reading_meaning| :|rmgroup|)
     ("ReadingMeaningGroup"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t))
     (:|reading|
      ("Reading"
       ("reading_meaning_group_id" "INTEGER NOT NULL REFERENCES ReadingMeaningGroup(id)" :parent-id
                                   :indexed t)
       ("reading" "TEXT NOT NULL COLLATE NOCASE" (:text) :indexed t)
       ("type" "TEXT NOT NULL" (:|r_type|) :indexed t)
       ("on_type" "TEXT" (:|on_type|))
       ("approved" "TEXT" (:|r_status|))))
     (:|meaning|
      ("Meaning"
       ("reading_meaning_group_id" "INTEGER NOT NULL REFERENCES ReadingMeaningGroup(id)" :parent-id
                                   :indexed t)
       ("meaning" "TEXT NOT NULL COLLATE NOCASE" (:text)
                  :indexed t)
       ("language" "TEXT NOT NULL" (:or (:|m_lang|) "en")
                   :comment "The two-letter language code of the meaning"
                   :indexed t))))
    ((:|reading_meaning| :|nanori|)
     ("Nanori"
      ("character_id" "INTEGER NOT NULL REFERENCES Character(id)" :parent-id
                      :indexed t)
      ("nanori" "TEXT NOT NULL" (:text) :indexed t))))
  "The structure of the Kanjidic XML file in the same format as *JMDICT-STRUCTURE*.")

(define-condition constraint-violation-error (error)
  ((column
    :initarg :column
    :initform (error "Must supply a column")
    :reader column
    :documentation "The name of the column violating the constraint")
   (constraint
    :initarg :constraint
    :initform (error "Must supply a constraint")
    :reader constraint
    :documentation "The constraint violated"))
  (:documentation "An error resulting from violating a constraint")
  (:report (lambda (error stream)
             (format stream "Constraint ~a violated for column ~a"
                     (constraint error) (column error)))))

(defun convert-xml-to-sqlite (xml-path sqlite-path structure)
  "Convert an XML file to a SQLite database following STRUCTURE.
STRUCTURE is a database structure in the format of
*JMDICT-STRUCTURE*."
  (sqlite:with-open-database (db sqlite-path)
    (create-tables db structure)
    (insert-values db xml-path structure)
    (sqlite:execute-non-query db "ANALYZE")))

(defun convert-tatoeba-to-sqlite (examples-path sqlite-path)
  "Convert the Tatoeba/Tanaka Corpus examples file to a SQLite database."
  ;; This isn't an XML file, so the XML structure stuff used for
  ;; JMDict and Kanjidic won't work
  (sqlite:with-open-database (db sqlite-path)
    (create-table db "Example"
                  '(("id" "INTEGER PRIMARY KEY")
                    ("japanese" "TEXT NOT NULL COLLATE NOCASE"
                     :indexed t)
                    ("english" "TEXT NOT NULL COLLATE NOCASE"
                     :indexed t)))
    (create-table db "Annotation"
                  '(("id" "INTEGER PRIMARY KEY")
                    ("example_id" "INTEGER NOT NULL REFERENCES Example(id)"
                     :indexed t)
                    ("word" "TEXT NOT NULL"
                     :comment "The word as it appears in JMDict")
                    ("reading" "TEXT"
                     :comment "The word's reading in hiragana as it appears in JMDict")
                    ("sense" "INTEGER"
                     :comment "A sense number in JMDict")
                    ("sentence_form" "TEXT"
                     :comment "The form of the word in the sentence")
                    ("checked" "INTEGER NOT NULL"
                     :comment "Whether this is a 'good' and checked example of the word")))
    (sqlite:with-transaction db
      (with-open-file (examples examples-path)
        (loop with a-line-scanner = (ppcre:create-scanner
                                     "^A: ([^\\t]+)\\t([^#]+)")
           and b-line-scanner = (ppcre:create-scanner
                                 "^B: (.*)$")
           and annotation-scanner = (ppcre:create-scanner
                                     "^([^\\[\\(\\{~]+)(?:\\(([^\\)]+)\\))?(?:\\[([^\\]]+)\\])?(?:\\{([^\\}]+)\\})?(~)?$")
           and insert-example = (prepare-non-query-statement
                                 db (make-insert-query "Example" 3))
           and insert-annotation = (prepare-non-query-statement
                                    db (make-insert-query "Annotation" 7))
           for n from 1
           for a-line = (read-line examples nil)
           for b-line = (read-line examples nil)
           while (and a-line b-line)
           do (ppcre:register-groups-bind (japanese english)
                  (a-line-scanner a-line)
                (funcall insert-example n japanese english)
                (ppcre:register-groups-bind (annotations)
                    (b-line-scanner b-line)
                  (loop for annotation in (uiop:split-string
                                           annotations :separator '(#\Space))
                     do (ppcre:register-groups-bind
                            (word reading sense sentence-form checked)
                            (annotation-scanner annotation)
                          (funcall insert-annotation
                                   nil n
                                   word reading sense sentence-form
                                   (if checked 1 0))))))
           when (= (rem n 10000) 0)
           do (format *error-output* "Processed ~d entries~%" n))))
    (sqlite:execute-non-query db "ANALYZE")))

;;; Macros and helper functions

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

(defun loop-substructures (path value function)
  "Execute FUNCTION with all sub-structures of VALUE by following PATH.
As a special case, PATH may be a single symbol.

For example, using this function with the path (:codepoint :cp_value)
will call FUNCTION with the contents of each cp_value element in all
the codepoint elements in VALUE."
  (ctypecase path
    (null (funcall function value))
    (symbol (loop-substructures (list path) value function))
    (list (loop for sub-value in (cdr (assoc (first path) value))
               do (loop-substructures (rest path) sub-value function)))))

;;; SQLite interaction

(defun create-tables (db structure)
  "Create the tables specified by STRUCTURE in DB, dropping them if they already exist."
  (destructuring-bind (element (table &rest columns)
                               &rest sub-structures)
      structure
    (declare (ignore element))
    (let ((columns
           (list*
            (list "id" "INTEGER PRIMARY KEY")
            (mapcar
             (lambda (column)
               (destructuring-bind (name type path &key comment indexed
                                         &allow-other-keys)
                   column
                 (declare (ignore path))
                 (list name type :comment comment :indexed indexed)))
             columns))))
      (create-table db table columns))
    (loop for sub-structure in sub-structures
       do (create-tables db sub-structure))))

(defun create-table (db table columns)
  "Create TABLE in DB with COLUMNS, dropping it if it exists.
Also create any indexes associated with COLUMNS. COLUMNS is a list
with elements of the form (NAME TYPE &key COMMENT INDEXED). Unlike
CREATE-TABLES, do not add any implicit ID column to the table."
  (sqlite:execute-non-query
   db (format nil "DROP TABLE IF EXISTS ~a" table))
  (let ((table-string
         (with-output-to-string (str)
           (format str "CREATE TABLE ~a (~%" table)
           (loop for rest-columns on columns
              do (destructuring-bind (name type &key comment &allow-other-keys)
                     (first rest-columns)
                   (format str "  ~a ~a" name type)
                   (unless (endp (rest rest-columns))
                     (format str ","))
                   (when comment
                     (format str " -- ~a" comment))
                   (format str "~%")))
           (format str ");"))))
    (sqlite:execute-non-query db table-string))
  (loop for column in columns
     do (destructuring-bind (name type &key indexed &allow-other-keys)
            column
          (declare (ignore type))
          (when indexed
            (create-index db table name)))))

(defun create-index (db table column)
  "Create an index on COLUMN of TABLE in DB, dropping it if it exists."
  (let ((index-name (format nil "~a_~a" table column)))
    (sqlite:execute-non-query
     db (format nil "DROP INDEX IF EXISTS ~a" index-name))
    (sqlite:execute-non-query
     db (format nil "CREATE INDEX ~a ON ~a(~a)" index-name table column))))

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
                                         "Processed ~d entries~%" n)))
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
          (id (incf (gethash table id-counters 0))))
      (handler-case
          (apply statement id
                 (mapcar (lambda (column)
                           (column-value column value parent-id))
                         columns))
        (constraint-violation-error (e)
          (format *error-output* "Warning: skipping record: ~a~%" e)))
      ;; Process sub-structures
      (loop for (path . substructure) in (rest structure)
         do (loop-substructures path value
                 (lambda (child-value)
                   (insert-value db child-value substructure
                                 id-counters prepared-statements
                                 :parent-id id)))))))

(defun column-value (column value parent-id)
  "Return the value of COLUMN evaluated in the context of VALUE.
Signal a condition of type CONSTRAINT-VIOLATION-ERROR if any
constraints on COLUMN are violated. Use PARENT-ID as the value of the
special :PARENT-ID path if requested."
  (destructuring-bind (name type path &key constraints &allow-other-keys)
      column
    (declare (ignore type))
    (let ((value (case path
                   (:parent-id parent-id)
                   (t (structure-path path value)))))
      (loop for constraint in constraints
         do (case constraint
              (:not-null (when (null value)
                           (error 'constraint-violation-error
                                  :column name
                                  :constraint constraint)))))
      value)))

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
the first element matching the tag extracted.

For example, the path (:k_ele :keb :TEXT) would return the first kanji
reading of an element. If the path cannot be followed completely,
return NIL.

Certain special 'operators' are supported:
(:OR PATH DEFAULT) - return DEFAULT instead of the value at PATH if the same is NIL
(:IF PATH TRUE-VALUE FALSE-VALUE) - if the value at PATH is not NIL, return TRUE-VALUE, else return FALSE-VALUE"
  (case (first path)
    (:or (destructuring-bind (path default) (rest path)
           (or (structure-path path structure) default)))
    (:if (destructuring-bind (path true-value false-value) (rest path)
           (if (structure-path path structure) true-value false-value)))
    (t (loop with result = structure
          for tag in path
          do (setf result (cadr (assoc tag result)))
          finally (return result)))))

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
                     (push value (cdr assoc)))
                   (cons (list key value) alist))))
           (convert-attributes (attributes)
             (mapcar (lambda (attr)
                       ;; Convert the cons cell representation to a
                       ;; list for consistency, even though there's
                       ;; only one value for each attribute
                       (cons (car attr) (list (cdr attr))))
                     attributes))
           (new-element-hook (name attributes seed)
             (declare (ignore name attributes seed))
             ())
           (reverse-assoc-pair (pair)
             (cons (car pair) (nreverse (cdr pair))))
           (finish-element-hook (name attributes parent-seed seed)
             (let ((structure (nconc (convert-attributes attributes)
                                     ;; All the value lists in the
                                     ;; seed are in reverse order
                                     (mapcar #'reverse-assoc-pair seed))))
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
  "Parse a stream of XML from INPUT and return a hashtable with all entities found in the DOCTYPE (as well as all standard XML entities).
Return NIL if there was no (valid) doctype anywhere in the file."
  ;; Skip input until we reach the DOCTYPE
  (loop with doctype-scanner = (ppcre:create-scanner "<!DOCTYPE")
     for line = (read-line input nil)
     unless line return nil
     until (ppcre:scan doctype-scanner line))
  ;; Try to read all the entities we can until we reach ]>
  ;; Unfortunately, make-standard-entities is not exported in s-xml
  (loop with entities = (s-xml::make-standard-entities)
     and entity-scanner = (ppcre:create-scanner
                           "<!ENTITY +([^ ]+) +\"([^\"]+)\">")
     and end-scanner = (ppcre:create-scanner "]>")
     for line = (read-line input nil)
     unless line return nil
     do (ppcre:register-groups-bind (entity expansion) (entity-scanner line)
          (setf (gethash entity entities) expansion))
     when (ppcre:scan end-scanner line)
     return entities))

;;;; jmdict.lisp ends here
