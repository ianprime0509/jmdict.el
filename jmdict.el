;;; -*- lexical-binding: t; -*-
;;; jmdict.el --- JMDict and Kanjidic interface for Emacs

;; Copyright 2019 Ian Johnson

;; Author: Ian Johnson <ianprime0509@gmail.com>
;; Version: 0.1.0
;; Keywords: tools
;; URL: https://github.com/ianprime0509/jmdict.el
;; Package-Requires: (cl-lib esqlite subr-x)

;; This file is not part of GNU Emacs.

;; This file is part of jmdict.el, a free software project for
;; integrating the JMDict and Kanjidic dictionaries into Emacs. You
;; are free to use, distribute and change the code in this project
;; subject to the MIT license included in the project root directory.

;;; Commentary:

;; This is an interface for the JMDict and Kanjidic Japanese
;; dictionary files, providing a convenient Japanese dictionary within
;; Emacs. Prior to using this package, the raw XML files must be
;; converted to SQLite databases using the supporting Common Lisp code
;; in the project root.

;;; Code:

;; Configuration

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)

(require 'esqlite)

(defgroup jmdict nil
  "An interface to the JMDict Japanese dictionary"
  :prefix "jmdict-"
  :group 'applications)

(defun jmdict--set-default-path (symbol path)
  "Set the default value of SYMBOL to PATH.
PATH is expanded using `expand-file-name'."
  (set-default symbol (expand-file-name path)))

(defcustom jmdict-jmdict-path
  (expand-file-name "~/jmdict/jmdict.sqlite3")
  "Path to the JMDict SQLite database."
  :group 'jmdict
  :type 'file
  :set #'jmdict--set-default-path)

(defcustom jmdict-kanjidic-path
  (expand-file-name "~/jmdict/kanjidic.sqlite3")
  "Path to the Kanjidic SQLite database."
  :group 'jmdict
  :type 'file
  :set #'jmdict--set-default-path)

(defcustom jmdict-tatoeba-path
  (expand-file-name "~/jmdict/tatoeba.sqlite3")
  "Path to the Tatoeba SQLite database."
  :group 'jmdict
  :type 'file
  :set #'jmdict--set-default-path)

(defcustom jmdict-result-limit 15
  "Maximum number of results to fetch for a query."
  :group 'jmdict
  :type 'integer)

(defface jmdict-header
  '((t . (:height 2.0 :weight bold)))
  "Face for JMDict entry headers."
  :group 'jmdict)

(defface jmdict-sub-header
  '((t . (:height 1.5 :weight bold)))
  "Face for JMDict sub-headers."
  :group 'jmdict)

;; Caching support

(defvar jmdict--caches ()
  "A list of caches currently defined.")

(defmacro jmdict--defun-cached (name arglist
                                     &optional docstring decl
                                     &rest body)
  "Define a function that caches its results.
Behave exactly like `defun', but also define a cache `NAME-cache'
and check the cache for the function's arguments to potentially
avoid a call to the underlying function."
  (declare (doc-string 3) (indent 2))
  ;; Normalize body forms
  (unless (eql 'declare (car-safe decl))
    (push decl body)
    (setf decl nil))
  (unless (stringp docstring)
    (push docstring body)
    (setf docstring nil))
  (let ((cache-var (intern (concat (symbol-name name) "-cache")))
        (cache-doc (format "Cache for %s." name))
        (arguments (cl-remove-if (lambda (arg)
                                   (or (eql '&optional arg)
                                       (eql '&rest arg)))
                                 arglist))
        (cached (gensym)))
    `(progn
       (defvar ,cache-var (make-ring 20) ,cache-doc)
       (push ',cache-var jmdict--caches)
       (defun ,name ,arglist
         ,docstring
         ,decl
         (if-let ((,cached
                   (cdr (assoc (list ,@arguments)
                               (ring-elements ,cache-var)))))
             ,cached
           (let ((,cached (progn ,@body)))
             (ring-insert ,cache-var
                          (cons (list ,@arguments) ,cached))
             ,cached))))))

(defun jmdict--clear-caches ()
  "Clear all caches."
  (dolist (cache-var jmdict--caches)
    (let ((size (ring-size (symbol-value cache-var))))
      (set cache-var (make-ring size)))))

;; SQLite integration

(defun jmdict--query (db spec where &optional limit order-by)
  "Query DB and return the resulting data as a structure.

WHERE is a filter expression provided in a simple s-expression
format (the only conversion is to add parentheses and put the
operator between the two operands if there are two operands).

LIMIT, if non-nil, is the maximum number of results to return.

ORDER-BY is a list of columns on which to order, described using
the same s-expression syntax as WHERE. If nil, order the columns
according to the primary keys of the tables in SPEC (in the order
they appear). The special value `:ids' will be substituted with
the ID columns of the tables in SPEC.

SPEC is a query specification, which takes the format (TABLE
PARENT-ID COLUMNS &rest SUB-STRUCTURES).

TABLE is the table from which to select COLUMNS. Every table is
assumed to have a primary key column named id. PARENT-ID is the
column in the child table referencing the primary key of the
parent (ignored at the top level). The first element of COLUMNS
may be :optional to indicate that a left join should be used
instead of an inner join, if applicable.

Each column in COLUMNS (besides any keywords) may be a string,
indicating a column to select from TABLE, or a list of the
form (EXPRESSION ALIAS) to select a raw SQL expression.

SUB-STRUCTURES is a list of structures in child tables to join
with the parent.

The return value is a list of alists, where each alist contains
the properties named by the columns and sub-tables are
represented as lists with the property name the same as the
sub-table name."
  (if (file-readable-p db)
      (let* ((query (jmdict--make-query spec where limit order-by))
             (rows-raw (esqlite-read db query))
             ;; Esqlite uses :null instead of nil for null values, so we
             ;; have to convert them all
             (rows (mapcar (lambda (row)
                             (mapcar (lambda (column)
                                       (if (eql :null column) nil column))
                                     row))
                           rows-raw)))
        (jmdict--parse-rows rows spec))
    (error "Database not readable: %s" db)))

(defun jmdict--parse-rows (rows spec)
  "Parse structured data from ROWS according to SPEC.
The return value is in the same format as `jmdict--query'."
  (cl-destructuring-bind (table parent-id columns &rest sub-specs)
      spec
    (declare (ignore table parent-id))
    ;; Ignore keyword options in columns
    (setf columns (cl-remove-if #'keywordp columns))
    (let ((grouped-rows
           (jmdict--group-by #'cl-first
                             (cl-remove-if (lambda (row)
                                             (null (cl-first row)))
                                           rows))))
      (mapcar (lambda (group)
                (jmdict--structure-rows (cdr group) columns sub-specs))
              grouped-rows))))

(defun jmdict--structure-rows (rows columns sub-specs)
  "A helper for `jmdict--parse-rows'.
ROWS are the rows to process, COLUMNS are the columns for the
top-level table and SUB-SPECS are the sub-tables to process."
  (let (parsed
        (consumed (1+ (length columns))))
    ;; Populate the initial parsed alist with the column values
    ;; of the first row
    (cl-loop for column in columns
             ;; We use rest below to ignore the initial (implicit) ID
             ;; column
             for value in (cl-rest (cl-first rows))
             do (push (cons column value) parsed))
    ;; Populate additional plist entries for sub-tables
    (dolist (sub-spec sub-specs)
      (let* ((rows (mapcar (lambda (row)
                             (nthcdr consumed row))
                           rows))
             (sub-value (jmdict--parse-rows rows sub-spec)))
        (cl-incf consumed (jmdict--n-columns sub-spec))
        (push (cons (cl-first sub-spec) sub-value) parsed)))
    parsed))

(defun jmdict--n-columns (spec)
  "Return the number of columns in SPEC.
This is the actual number of columns in the query, including any
ID columns that are added implicitly."
  (multiple-value-bind (tables columns)
      (jmdict--parse-query-spec spec nil)
    (declare (ignore tables))
    (length columns)))

(defun jmdict--make-query (spec where &optional limit order-by)
  "Return the SQL query corresponding to the given arguments.
SPEC, WHERE, LIMIT and ORDER-BY are as described in
`jmdict--query'."
  (cl-multiple-value-bind (tables columns)
      (jmdict--parse-query-spec spec nil)
    (let* ((columns (cl-loop for (expr alias) in columns
                             collect (concat expr " AS " alias)))
           (first-table (cl-first (cl-first tables)))
           (rest-tables
            (cl-loop for (table join-on optional) in (cl-rest tables)
                     collect (format "%s %s ON %s"
                                     (if optional "LEFT JOIN" "JOIN")
                                     table
                                     join-on)))
           (id-columns (mapcar (lambda (table)
                                 (format "%s.id" (cl-first table)))
                               tables))
           (order-columns
            (if order-by
                (mapcan (lambda (expr)
                          (if (eql :ids expr)
                              id-columns
                            (list (jmdict--convert-sql-expression expr))))
                        order-by)
              id-columns)))
      (format "SELECT %s\nFROM %s\n%s\nWHERE %s\nORDER BY %s%s;"
              (string-join columns ", ")
              first-table
              (string-join rest-tables "\n")
              (jmdict--convert-sql-expression where)
              (string-join order-columns ", ")
              (if limit (format "\nLIMIT %d" limit) "")))))

(defun jmdict--convert-sql-expression (expr)
  "Convert EXPR to a SQL expression.
EXPR is just a normal SQL expression, but written in s-expression
format. Unary and binary operators are converted in the natural
way; expressions with more than two operands are converted to
multiple applications of a binary operator from left to right."
  (if (not (listp expr)) (format "%s" expr)
    (cl-destructuring-bind (op &rest operands) expr
      (case (length operands)
        (0 (error "No operands provided"))
        (1 (format "(%s %s)"
                   op
                   (jmdict--convert-sql-expression (cl-first operands))))
        (2 (format "(%s %s %s)"
                   (jmdict--convert-sql-expression (cl-first operands))
                   op
                   (jmdict--convert-sql-expression (second operands))))
        (t (cl-destructuring-bind (op first second &rest rest) expr
             (jmdict--convert-sql-expression (list* op
                                                    (list op first second)
                                                    rest))))))))

(defun jmdict--parse-query-spec (spec parent)
  "Parse SPEC and return a list of tables and columns.
The returned list is of the form (TABLES COLUMNS), where each
element in TABLES is of the form (NAME JOIN-ON OPTIONAL) and each
element in columns is of the form (COLUMN-QUERY ALIAS).

PARENT is the name of the parent table, or nil if there is none."
  (cl-destructuring-bind (table parent-id columns &rest sub-specs)
      spec
    (let ((tables
           (list (list table
                       (when parent
                         (format "%s.%s = %s.id" table parent-id parent))
                       (eql :optional (cl-first columns)))))
          (columns
           (cl-loop for column in columns
                    when (stringp column)
                    collect (list (concat table "." column) column)
                    when (listp column)
                    collect (cl-destructuring-bind (expr alias) column
                              (list (jmdict--convert-sql-expression expr)
                                    alias)))))
      (push (list (concat table ".id") "id") columns)
      (dolist (sub-spec sub-specs)
        (cl-multiple-value-bind (sub-tables sub-columns)
            (jmdict--parse-query-spec sub-spec table)
          (setf tables (nconc tables sub-tables))
          (setf columns (nconc columns sub-columns))))
      (cl-values tables columns))))

(jmdict--defun-cached jmdict--get-entries (ids)
  "Find all JMdict entries with IDs in IDS.
IDS is a list of lists; all entries in a sub-list will be grouped
together in the returned results."
  (let ((group-expr
         (format "CASE\n%s\nEND"
                 (string-join
                  (cl-loop for group-num from 0
                           for id-group in ids
                           collect (format "WHEN Entry.id IN (%s) THEN %d"
                                           (string-join id-group ", ")
                                           group-num))
                  "\n")))
        (all-ids (apply #'append ids)))
    (jmdict--query
     jmdict-jmdict-path
     `("Entry" nil ((,group-expr "sort_group"))
       ("Kanji" "entry_id" (:optional "reading")
        ("KanjiInfo" "kanji_id" (:optional "info"))
        ("KanjiPriority" "kanji_id" (:optional "priority")))
       ("Reading" "entry_id" ("reading")
        ("ReadingRestriction" "reading_id" (:optional "restriction"))
        ("ReadingInfo" "reading_id" (:optional "info"))
        ("ReadingPriority" "reading_id" (:optional "priority")))
       ("Sense" "entry_id" ()
        ("SenseKanjiRestriction" "sense_id" (:optional "restriction"))
        ("SenseKanaRestriction" "sense_id" (:optional "restriction"))
        ("SenseCrossReference" "sense_id" (:optional "target"))
        ("SenseAntonym" "sense_id" (:optional "target"))
        ("SensePartOfSpeech" "sense_id" (:optional "part_of_speech"))
        ("SenseField" "sense_id" (:optional "field"))
        ("SenseMisc" "sense_id" (:optional "info"))
        ("SenseSource" "sense_id" (:optional "word" "language"))
        ("SenseDialect" "sense_id" (:optional "dialect"))
        ("Gloss" "sense_id" ("gloss" "type"))
        ("SenseInfo" "sense_id" (:optional "info"))))
     `(and (in "Entry.id" ,(format "(%s)" (string-join all-ids ", ")))
           (= "Gloss.language" "'eng'"))
     nil
     '("sort_group" :ids))))

(jmdict--defun-cached jmdict--get-kanji (character)
  "Find the Kanjidic entry for CHARACTER."
  (cl-first
   (jmdict--query
    jmdict-kanjidic-path
    '("Character" nil ("literal" "grade" "stroke_count" "frequency" "jlpt_level")
      ("Radical" "character_id" (:optional "radical" "type"))
      ("Variant" "character_id" (:optional "variant" "type"))
      ("DictionaryReference" "character_id" (:optional "reference" "type" "volume" "page"))
      ("ReadingMeaningGroup" "character_id" ()
       ("Reading" "reading_meaning_group_id" ("reading" "type" "on_type" "approved"))
       ("Meaning" "reading_meaning_group_id" ("meaning")))
      ("Nanori" "character_id" (:optional "nanori")))
    `(and (= "Character.literal" ,(esqlite-format-text character))
          (in "Reading.type" "('ja_kun', 'ja_on')")
          (= "Meaning.language" "'en'")))))

(defun jmdict--search-entries-by-kanji (query limit)
  "Search for JMDict entries where QUERY matches a kanji reading.
LIMIT is the maximum number of results to return."
  (jmdict--query jmdict-jmdict-path
                 '("Entry" nil ("id")
                   ("Kanji" "entry_id" ()))
                 `(like "Kanji.reading" ,(esqlite-format-text query))
                 limit))

(defun jmdict--search-entries-by-kana (query limit)
  "Search for JMDict entries where QUERY matches a kana reading.
LIMIT is the maximum number of results to return."
  (jmdict--query jmdict-jmdict-path
                 '("Entry" nil ("id")
                   ("Reading" "entry_id" ()))
                 `(like "Reading.reading" ,(esqlite-format-text query))
                 limit))

(defun jmdict--search-entries-by-gloss (query limit)
  "Search for JMDict entries where QUERY matches a gloss.
LIMIT is the maximum number of results to return."
  (jmdict--query jmdict-jmdict-path
                 '("Entry" nil ("id")
                   ("Sense" "entry_id" ()
                    ("Gloss" "sense_id" ())))
                 `(and (like "Gloss.gloss" ,(esqlite-format-text query))
                       (= "Gloss.language" "'eng'"))
                 limit))

(defun jmdict--try-queries (methods query-string limit)
  "Try queries from METHODS in order.
LIMIT is the maximum number of results to return. Each element of
METHODS is a list of the form (PRECONDITION QUERY-FUNCTION),
where the query will only be tried if PRECONDITION is non-nil and
QUERY-FUNCTION will be called with QUERY-STRING and the remaining
limit to return a list of results.

Return two 'values' (really a list of two elements using
`cl-values'): the results as a list of lists (each sub-list
contains the results of a different query) and the total number
of results."
  (cl-loop with found = 0
           for (condition query-function) in methods
           when (and condition (< found limit))
           collect (let ((matches (funcall query-function query-string
                                           (- limit found))))
                     (cl-incf found (length matches))
                     matches)
           into results
           finally (return (cl-values results found))))

(jmdict--defun-cached jmdict--search-entries (query)
  "Search for JMDict entries matching QUERY.
The return value is a list of entry IDs."
  (let* ((query-text (jmdict--strip-wildcards query))
         (try-kanji (jmdict--contains-japanese query-text))
         (try-kana (jmdict--is-kana-only query-text))
         (try-gloss (not (jmdict--contains-japanese query-text)))
         (try-wildcards (not (jmdict--contains-wildcard query)))
         (query-right-wild (concat query "%"))
         (query-left-wild (concat "%" query))
         (query-methods
          ;; A list of queries to try, in order, until we reach the
          ;; maximum number of results. Each method is a list of the
          ;; form (PRECONDITION QUERY-FUNCTION).
          (list
           (list try-kanji #'jmdict--search-entries-by-kanji)
           (list try-kana #'jmdict--search-entries-by-kana)
           (list try-gloss #'jmdict--search-entries-by-gloss))))
    (cl-multiple-value-bind (results n-found)
        (jmdict--try-queries query-methods query jmdict-result-limit)
      ;; Try additional wildcards, if applicable
      (when try-wildcards
        (let ((results-remaining (- jmdict-result-limit n-found)))
          ;; Try wildcard on the right ("starts with")
          (when (cl-plusp results-remaining)
            (cl-multiple-value-bind (right-matches n-matches)
                (jmdict--try-queries query-methods query-right-wild
                                     results-remaining)
              (cl-decf results-remaining n-matches)
              (setf results (nconc results right-matches))))
          ;; Try wildcard on the left ("ends with")
          (when (cl-plusp results-remaining)
            (setf results
                  (nconc results
                         ;; If this were Common Lisp, we wouldn't need
                         ;; any extra code here to just extract the
                         ;; first return value :(
                         (cl-first (jmdict--try-queries query-methods
                                                        query-left-wild
                                                        results-remaining)))))))
      (mapcar (lambda (group)
                (mapcar (lambda (entry) (cdr (assoc "id" entry))) group))
              results))))

;; Utility functions

(defconst jmdict--kanji-reading-types
  '(("ja_on" . "on")
    ("ja_kun" . "kun"))
  "Alist of Kanjidic reading types and descriptions.")

(defun jmdict--is-wildcard (char)
  "Return non-nil if CHAR is a wildcard."
  (or (eql ?_ char) (eql ?% char)))

(defun jmdict--contains-wildcard (query)
  "Return non-nil if QUERY contains a wildcard."
  (cl-loop for char across query
           thereis (jmdict--is-wildcard char)))

(defun jmdict--strip-wildcards (query)
  "Return QUERY with all wildcard characters removed."
  (cl-remove-if (lambda (char)
                  (or (eql ?_ char) (eql ?% char)))
                query))

(defun jmdict--is-japanese (char)
  "Return non-nil if CHAR is a Japanese character."
  (seq-contains '(kana han cjk-misc) (aref char-script-table char)))

(defun jmdict--contains-japanese (query)
  "Return non-nil if QUERY contains a Japanese character."
  (cl-loop for char across query
           thereis (jmdict--is-japanese char)))

(defun jmdict--is-kana (char)
  "Return non-nil if CHAR is a kana."
  (eql 'kana (aref char-script-table char)))

(defun jmdict--is-kana-only (query)
  "Return non-nil if QUERY contains only kana."
  (cl-loop for char across query
           always (jmdict--is-kana char)))

(defun jmdict--is-kanji (char)
  "Return non-nil if CHAR is a kanji."
  (eql 'han (aref char-script-table char)))

(defun jmdict--contains-kanji (query)
  "Return non-nil if QUERY contains a kanji."
  (cl-loop for char across query
           thereis (jmdict--is-kanji char)))

(defun jmdict--primary-reading (entry)
  "Return the primary reading for ENTRY.
The primary reading is the first kanji if any kanji are available
or the first kana reading."
  (or (cdr (assoc "reading" (cl-first (cdr (assoc "Kanji" entry)))))
      (cdr (assoc "reading" (cl-first (cdr (assoc "Reading" entry)))))))

(defun jmdict--group-by (function list)
  "Group LIST by the values extracted by FUNCTION.
Return an alist where the keys are the values extracted by
FUNCTION and the values are lists of the corresponding elements
of LIST."
  (let (groups)
    (dolist (element list)
      (let ((key (funcall function element)))
        (if-let ((group (assoc key groups)))
            (push element (cdr group))
          (push (cons key (list element)) groups))))
    (nreverse (mapcar (lambda (group) (cons (car group)
                                            (nreverse (cdr group))))
                      groups))))

(defun jmdict--values (path alist &optional multiple)
  "Extract a list of the values at PATH in ALIST.
If PATH is a single value, behave as `alist-get' with `equal' as
the test function but return a list rather than a single value.
If PATH is a list, follow the components of PATH sequentially in
the same fashion.

If MULTIPLE is non-nil, treat ALIST as a list of alists and
return a list of all values found."
  (if (listp path)
      (if (cl-endp path)
          alist
        (jmdict--values (cl-rest path)
                        (jmdict--values (cl-first path) alist multiple)
                        t))
    (if multiple
        (apply #'append
               (mapcar
                (lambda (alist)
                  (let ((value (cdr (assoc path alist))))
                    (if (listp value) value (list value))))
                alist))
      (jmdict--values path (list alist) t))))

;; JMDict buffer display

(defvar jmdict--buffer-name "*Jmdict*")

(defvar jmdict--kanji-buffer-name "*Jmdict kanji*")

(define-button-type 'jmdict-reference
  'help-echo "mouse-2, RET: Follow reference"
  'follow-link t
  'action #'jmdict--follow-reference)

(defconst jmdict--reference-separator "・"
  "The string that separates parts of a reference.")

(defun jmdict--follow-reference (button)
  "Follow the reference linked by BUTTON."
  ;; TODO: not all references are just a single word; some reference
  ;; only part of a definition
  (jmdict (cl-first (split-string (button-label button)
                               jmdict--reference-separator))))

(defun jmdict--insert-entry-header (header)
  "Insert HEADER as an entry header.
This adds text properties to HEADER so that the `jmdict-header'
face is used and a special `jmdict-header' property is present to
make it easier to identify headers."
  (insert (propertize header
                      'face 'jmdict-header
                      'jmdict-header t)))

(defun jmdict--insert-entry (entry)
  "Insert ENTRY into the current buffer."
  ;; Readings
  (let* ((primary (jmdict--primary-reading entry))
         (kanji-readings
          (cl-remove-if (lambda (s) (equal (cdr (assoc "reading" s))
                                           primary))
                        (cdr (assoc "Kanji" entry))))
         (kana-readings
          (cl-remove-if (lambda (r) (equal (cdr (assoc "reading" r))
                                           primary))
                        (cdr (assoc "Reading" entry)))))
    (jmdict--insert-entry-header primary)
    (insert "\n")
    (dolist (kanji kanji-readings)
      (insert (propertize (cdr (assoc "reading" kanji))
                          'face 'jmdict-sub-header))
      (when-let ((info (jmdict--values '("KanjiInfo" "info") kanji)))
        (insert " (" (string-join info ", ") ")"))
      (insert "\n"))
    (dolist (kana kana-readings)
      (insert (propertize (cdr (assoc "reading" kana))
                          'face 'bold))
      (when-let ((info (jmdict--values '("ReadingInfo" "info") kana)))
        (insert " (" (string-join info ", ") ")"))
      (when-let ((restrictions
                  (jmdict--values '("ReadingRestriction" "restriction") kana)))
        (insert " (applies only to " (string-join restrictions ", ") ")"))
      (insert "\n")))
  (insert "\n")
  ;; Senses
  (dolist (sense (cdr (assoc "Sense" entry)))
    (when-let ((parts-of-speech
                (jmdict--values '("SensePartOfSpeech" "part_of_speech") sense)))
      (insert (propertize (string-join parts-of-speech "\n")
                          'face 'bold)
              "\n"))
    (when-let ((fields
                (jmdict--values '("SenseField" "field") sense)))
      (insert (propertize (string-join fields ", ")
                          'face 'italic)
              "\n"))
    (when-let ((misc
                (jmdict--values '("SenseMisc" "info") sense)))
      (insert (propertize (string-join misc "\n")
                          'face 'italic)
              "\n"))
    (when-let ((info
                (jmdict--values '("SenseInfo" "info") sense)))
      (insert (propertize (string-join info "\n")
                          'face 'italic)
              "\n"))
    (when-let ((sources
                (mapcar (lambda (s)
                          (format "%s (%s)"
                                  (cdr (assoc "word" s))
                                  (cdr (assoc "language" s))))
                        (jmdict--values "SenseSource" sense))))
      (insert "Source language: " (string-join sources ", ")))
    (when-let ((dialects
                (jmdict--values '("SenseDialect" "dialect") sense)))
      (insert "Dialects: " (string-join dialects ", ")))
    (when-let ((antonyms
                (jmdict--values '("SenseAntonym" "target") sense)))
      (insert "Antonyms: ")
      (cl-loop for ant on antonyms
               do (insert-button (cl-first ant) 'type 'jmdict-reference)
               unless (cl-endp (cl-rest ant)) do (insert ", "))
      (insert "\n"))
    (when-let ((cross-references
                (jmdict--values '("SenseCrossReference" "target") sense)))
      (insert "See also: ")
      (cl-loop for ref on cross-references
               do (insert-button (cl-first ref) 'type 'jmdict-reference)
               unless (cl-endp (cl-rest ref)) do (insert ", "))
      (insert "\n"))
    (let ((glosses (jmdict--values '("Gloss" "gloss") sense)))
      (insert " - " (string-join glosses ", ") "\n"))))

(defun jmdict--insert-kanji (kanji)
  "Insert KANJI into the current buffer."
  (jmdict--insert-entry-header (cdr (assoc "literal" kanji)))
  (insert "\n")
  (let ((stroke-count (cdr (assoc "stroke_count" kanji))))
    (insert (propertize
             (format "%s stroke%s\n"
                     stroke-count
                     (if (equal "1" stroke-count) "" "s"))
             'face 'bold)))
  (insert "\n")
  (dolist (reading-meaning-group (jmdict--values "ReadingMeaningGroup" kanji))
    (let* ((readings (jmdict--values "Reading" reading-meaning-group))
           (grouped-readings (jmdict--group-by (lambda (reading)
                                                 (cdr (assoc "type" reading)))
                                               readings))
           (sorted-readings (cl-sort grouped-readings #'string<
                                     :key #'car)))
      (dolist (group sorted-readings)
        (let ((readings (mapcar (lambda (reading)
                                  (cdr (assoc "reading" reading)))
                                (cdr group))))
          (insert (format "%s (%s)\n"
                          (propertize (string-join readings ", ")
                                      'face 'jmdict-sub-header)
                          (cdr (assoc (car group)
                                      jmdict--kanji-reading-types)))))))
    (insert "\n")
    (dolist (meaning
             (jmdict--values '("Meaning" "meaning") reading-meaning-group))
      (insert " - " meaning "\n"))
    (insert "\n"))
  (when-let ((references (jmdict--values "DictionaryReference" kanji)))
    (insert (propertize "Dictionary references:" 'face 'bold) "\n")
    (dolist (reference references)
      (insert (format "%s: %s\n"
                      (propertize (cdr (assoc "type" reference))
                                  'face 'bold)
                      (cdr (assoc "reference" reference))))))
  (insert "\n"))

(defmacro jmdict--with-jmdict-buffer (name &rest body)
  "Evaluate BODY with the JMDict buffer as the current buffer.
The JMDict buffer will have the name NAME and be created in
`jmdict-mode'."
  (declare (indent 1))
  (let ((buffer (gensym)))
    `(let ((,buffer (get-buffer ,name)))
       (unless ,buffer
         (setf ,buffer (get-buffer-create ,name))
         (with-current-buffer ,buffer
           (jmdict-mode)))
       (with-current-buffer ,buffer
         ,@body
         (set-buffer-modified-p nil)))))

;; Interactive commands

(defvar jmdict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'jmdict)
    (define-key map (kbd "k") #'jmdict-kanji)
    (define-key map (kbd "l") #'jmdict-go-back)
    (define-key map (kbd "r") #'jmdict-go-forward)
    (define-key map (kbd "n") #'jmdict-next-entry)
    (define-key map (kbd "p") #'jmdict-previous-entry)
    map))

(defvar-local jmdict--history-back-stack ()
  "The stack of previous history entries.
Each element on the stack is of the form (TYPE QUERY POSITION),
where TYPE is either `entry' or `kanji', QUERY is the query
associated with the buffer and POSITION is the position of
point.")

(defvar-local jmdict--history-forward-stack ()
  "The stack of next history entries.
Each element on the stack is of the same form as
`jmdict--history-back-stack'.")

(defvar-local jmdict--current-query nil
  "The query associated with the current buffer.
This is a cons cell where the car is the type, either `entry' or
`kanji', and the cdr is the query string.")

(defun jmdict--move-beginning-of-header ()
  "Move to the beginning of the header under point.
If point is not on a header, do nothing."
  (when (or (eql (point-min) (point))
            (get-text-property (1- (point)) 'jmdict-header))
    (goto-char (or (previous-single-property-change (point) 'jmdict-header)
                   (point-min)))))

(defun jmdict--move-end-of-header ()
  "Move to the end of the header under point.
If point is not on a header, do nothing."
  (when (get-text-property (point) 'jmdict-header)
    (when-let ((end (next-single-property-change (point) 'jmdict-header)))
      (goto-char end))))

(defun jmdict--current-history-entry ()
  "Return a history entry representing the current state.
If there is no current query, return nil."
  (when jmdict--current-query
    (list (car jmdict--current-query)
          (cdr jmdict--current-query)
          (point))))

(defun jmdict--go (history-entry &optional preserve-history)
  "Display HISTORY-ENTRY in the current buffer.
HISTORY-ENTRY is a list of the form used in
`jmdict--history-back-stack' and `jmdict--history-forward-stack'.
If PRESERVE-HISTORY is non-nil, do not update the history.
Otherwise, clear all forward history and add the current query to
the back history. Regardless of the value of PRESERVE-HISTORY,
set `jmdict--current-query'."
  (let ((current-entry (jmdict--current-history-entry)))
    (cl-destructuring-bind (type query point) history-entry
      (let ((inhibit-read-only t))
       (erase-buffer)
       (cl-case type
         (entry
          (if-let* ((ids (jmdict--search-entries query))
                    (entries (jmdict--get-entries ids)))
            (dolist (entry entries)
              (jmdict--insert-entry entry)
              (insert "\n"))
            (error "No entry for %s" query)))
         (kanji
          (if-let* ((kanji (jmdict--get-kanji query)))
              (jmdict--insert-kanji kanji)
            (error "No kanji for %s" query)))))
      (goto-char (or point (point-min)))
      ;; Update the current query and history
      (setf jmdict--current-query (cons type query))
      (unless (or (null current-entry) preserve-history)
        (push current-entry jmdict--history-back-stack)
        (setf jmdict--history-forward-stack ())))))

(defun jmdict-go-back ()
  "Go back to the results of the previous query."
  (interactive)
  (if-let ((back (cl-first jmdict--history-back-stack)))
      (let ((current-entry (jmdict--current-history-entry)))
        (jmdict--go back t)
        (recenter)
        ;; Only update the history after navigation was successful
        (pop jmdict--history-back-stack)
        (push current-entry jmdict--history-forward-stack))
    (error "No previous query")))

(defun jmdict-go-forward ()
  "Go forward to the results of the next query.
This undoes the action of `jmdict-go-back'."
  (interactive)
  (if-let ((forward (cl-first jmdict--history-forward-stack)))
      (let ((current-entry (jmdict--current-history-entry)))
        (jmdict--go forward t)
        (recenter)
        ;; Only update the history after navigation was successful
        (pop jmdict--history-forward-stack)
        (push current-entry jmdict--history-back-stack))
    (error "No next query")))

(defun jmdict-next-entry (arg)
  "Navigate ARG entries forward.
Also recenter the window so that the entry header is at the top
of the window."
  (interactive "p")
  (when (cl-minusp arg)
    (jmdict-previous-entry (- arg)))
  (cl-loop repeat arg
           do (jmdict--move-end-of-header)
           (when-let ((end
                       (next-single-property-change (point)
                                                    'jmdict-header)))
             (goto-char end))
           (jmdict--move-beginning-of-header))
  (recenter 0))

(defun jmdict-previous-entry (arg)
  "Navigate ARG entries backwards.
Also recenter the window so that the entry header is at the top
of the window."
  (interactive "p")
  (when (cl-minusp arg)
    (jmdict-next-entry (- arg)))
  (cl-loop repeat arg
           do (jmdict--move-beginning-of-header)
           (when-let ((end
                       (previous-single-property-change (point)
                                                        'jmdict-header)))
             (goto-char end))
           (jmdict--move-beginning-of-header))
  (recenter 0))

;; Main function and mode

(defun jmdict (query)
  "Query JMDict for QUERY and display the results."
  (interactive
   (let* ((current-word (thing-at-point 'word t))
          (default (unless (or (null current-word)
                               (string-blank-p current-word))
                     current-word))
          (prompt (if default
                      (format "Query (default %s): " default)
                    "Query: ")))
     (list (read-string prompt nil nil default))))
  (jmdict--with-jmdict-buffer jmdict--buffer-name
    (jmdict--go (list 'entry query nil))
    (display-buffer (current-buffer))))

(defun jmdict-kanji (query)
  "Query Kanjidic for QUERY and display the results.
If no prefix argument is provided and a kanji is after point,
query for that kanji without prompting."
  (interactive
   (let* ((current-char (char-after))
          (default (when (jmdict--is-kanji current-char)
                     (string current-char)))
          (prompt (if default
                      (format "Kanji (default %s): " default)
                    "Kanji: ")))
     (if (and default (not current-prefix-arg))
         (list default)
       (list (read-string prompt nil nil default)))))
  (let ((display-action (when (eql 'jmdict-mode major-mode)
                          (list #'display-buffer-below-selected))))
    (jmdict--with-jmdict-buffer jmdict--kanji-buffer-name
      (jmdict--go (list 'kanji query nil))
      (display-buffer (current-buffer) display-action))))

(define-derived-mode jmdict-mode special-mode "JMDict"
  "Major mode for JMDict definitions."
  (buffer-disable-undo))

(provide 'jmdict)
;;; jmdict.el ends here
