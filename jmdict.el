;;; jmdict.el --- JMDict and Kanjidic interface for Emacs

;; Copyright 2019 Ian Johnson

;; This file is part of jmdict.el, a free software project for
;; integrating the JMDict and Kanjidic dictionaries into Emacs. You
;; are free to use, distribute and change the code in this project
;; subject to the MIT license included in the project root directory.

;;; Commentary:

;; TODO

;;; Code:

;; Configuration

(require 'cl-lib)
(require 'esqlite)
(require 'subr-x)

(defgroup jmdict nil
  "An interface to the JMDict Japanese dictionary"
  :prefix "jmdict-"
  :group 'applications)

(defcustom jmdict-jmdict-path
  "jmdict.sqlite3"
  "Path to the JMDict SQLite database."
  :group 'jmdict
  :type 'file)

(defface jmdict-header
  '((t . (:height 2.0 :weight bold)))
  "Face for JMDict entry headers."
  :group 'jmdict)

(defface jmdict-kanji
  '((t . (:height 1.5 :weight bold)))
  "Face for JMDict non-primary kanji."
  :group 'jmdict)

(defface jmdict-kana
  '((t . (:weight bold)))
  "Face for JMDict kana readings."
  :group 'jmdict)

(defvar jmdict--buffer-name "*Jmdict*")

(defvar jmdict-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;; SQLite integration

(defun jmdict--query (db spec where)
  "Query DB and return the resulting data as a structure.

WHERE is a filter expression provided in a simple s-expression
format (the only conversion is to add parentheses and put the
operator between the two operands if there are two operands).

SPEC is a query specification, which takes the format (TABLE
PARENT-ID COLUMNS &rest SUB-STRUCTURES).

TABLE is the table from which to select COLUMNS. Every table is
assumed to have a primary key column named id. PARENT-ID is the
column in the child table referencing the primary key of the
parent (ignored at the top level).

SUB-STRUCTURES is a list of structures in child tables to join
with the parent.

The return value is a list of alists, where each alist contains
the properties named by the columns and sub-tables are
represented as lists with the property name the same as the
sub-table name."
  (let* ((rows-raw (esqlite-read db (jmdict--make-query spec where)))
         ;; Esqlite uses :null instead of nil for null values, so we
         ;; have to convert them all
         (rows (mapcar (lambda (row)
                         (mapcar (lambda (column)
                                   (if (eql :null column) nil column))
                                 row))
                       rows-raw)))
    (jmdict--parse-rows rows spec)))

(defun jmdict--parse-rows (rows spec)
  "Parse structured data from ROWS according to SPEC.
The return value is in the same format as `jmdict--query'."
  (cl-destructuring-bind (table parent-id columns &rest sub-specs)
      spec
    (declare (ignore table parent-id))
    (let ((grouped-rows (jmdict--group-by-id rows)))
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
    (cl-mapcar (lambda (column value)
                 (setq parsed (cl-acons column value parsed)))
               ;; We use rest below to ignore the initial (implicit)
               ;; ID column
               columns (cl-rest (first rows)))
    ;; Populate additional plist entries for sub-tables
    (dolist (sub-spec sub-specs)
      (let* ((rows (mapcar (lambda (row)
                             (nthcdr consumed row))
                           rows))
             (sub-value (jmdict--parse-rows rows sub-spec)))
        (cl-incf consumed (jmdict--n-columns sub-spec))
        (setq parsed (cl-acons (first sub-spec) sub-value parsed))))
    parsed))

(defun jmdict--n-columns (spec)
  "Return the number of columns in SPEC.
This is the actual number of columns in the query, including any
ID columns that are added implicitly."
  (multiple-value-bind (tables columns)
      (jmdict--parse-query-spec spec nil)
    (declare (ignore tables))
    (length columns)))

(defun jmdict--group-by-id (rows)
  "Group ROWS by the first column in each row.
The order of the rows is preserved, and the return value is an
alist."
  (let (groups)
    (dolist (row rows)
      ;; Exclude rows where the ID is null
      (when (first row)
        (if-let ((assoc (assoc (first row) groups)))
            (push row (cdr assoc))
          (push (cons (first row) (list row)) groups))))
    ;; Make sure each group is in order
    (dolist (group groups)
      (setf (cdr group) (nreverse (cdr group))))
    (nreverse groups)))

(defun jmdict--make-query (spec where)
  "Return the SQL query corresponding to SPEC and WHERE.
SPEC and WHERE are as described in `jmdict--query'."
  (cl-multiple-value-bind (tables columns)
      (jmdict--parse-query-spec spec nil)
    (let ((columns (mapcar (lambda (column)
                             (format "%s.%s"
                                     (first column)
                                     (second column)))
                           columns))
          (first-table (first (first tables)))
          (rest-tables
           (cl-loop for (table join-on) in (cl-rest tables)
                    collect (format "LEFT JOIN %s ON %s"
                                    table
                                    join-on)))
          (id-columns (mapcar (lambda (table)
                                (format "%s.id" (first table)))
                              tables)))
      (format "SELECT %s\nFROM %s\n%s\nWHERE %s\nORDER BY %s;"
              (string-join columns ", ")
              first-table
              (string-join rest-tables "\n")
              (jmdict--convert-sql-expression where)
              (string-join id-columns ", ")))))

(defun jmdict--convert-sql-expression (expr)
  "Convert EXPR to a SQL expression.
EXPR is just a normal SQL expression, but written in s-expression
format."
  (if (not (listp expr)) (format "%s" expr)
    (cl-destructuring-bind (op &rest operands) expr
      (case (length operands)
        (1 (format "(%s %s)"
                   op
                   (jmdict--convert-sql-expression (first operands))))
        (2 (format "(%s %s %s)"
                   (jmdict--convert-sql-expression (first operands))
                   op
                   (jmdict--convert-sql-expression (second operands))))
        (t (error "Expressions with %d operands are not supported"
                  (length operands)))))))

(defun jmdict--parse-query-spec (spec parent)
  "Parse SPEC and return a list of tables and columns.
The returned list is of the form (TABLES COLUMNS), where each
element in TABLES is of the form (NAME JOIN-ON) and each element
in columns is of the form (TABLE COLUMN).

PARENT is the name of the parent table, or nil if there is none."
  (cl-destructuring-bind (table parent-id columns &rest sub-specs)
      spec
    (let ((tables
           (list (list table
                       (when parent
                         (format "%s.%s = %s.id" table parent-id parent)))))
          (columns
           (nconc (list (list table "id"))
                  (mapcar (lambda (column) (list table column))
                          columns))))
      (dolist (sub-spec sub-specs)
        (cl-multiple-value-bind (sub-tables sub-columns)
            (jmdict--parse-query-spec sub-spec table)
          (setq tables (nconc tables sub-tables))
          (setq columns (nconc columns sub-columns))))
      (cl-values tables columns))))

(defun jmdict--get-entries (ids)
  "Find all JMdict entries with IDs in IDS."
  (jmdict--query
   jmdict-jmdict-path
   '("Entry" nil ()
     ("Kanji" "entry_id" ("reading")
      ("KanjiInfo" "kanji_id" ("info")))
     ("Reading" "entry_id" ("reading")
      ("ReadingRestriction" "reading_id" ("restriction"))
      ("ReadingInfo" "reading_id" ("info")))
     ("Sense" "entry_id" ()
      ("SenseKanjiRestriction" "sense_id" ("restriction"))
      ("SenseKanaRestriction" "sense_id" ("restriction"))
      ("SenseCrossReference" "sense_id" ("target"))
      ("SenseAntonym" "sense_id" ("target"))
      ("SensePartOfSpeech" "sense_id" ("part_of_speech"))
      ("SenseField" "sense_id" ("field"))
      ("Gloss" "sense_id" ("gloss" "type"))))
   `(and (in "Entry.id" ,(format "(%s)" (string-join ids ", ")))
         (= "Gloss.language" "'eng'"))))

(defun jmdict--search-entries (query)
  "Search for JMDict entries matching QUERY.
The return value is a list of entry IDs."
  (let* ((query-string (esqlite-format-text query))
         (query-results
          (jmdict--query
           jmdict-jmdict-path
           '("Entry" nil ("id")
             ("Kanji" "entry_id" ())
             ("Reading" "entry_id" ()))
           `(or (= "Kanji.reading" ,query-string)
                (= "Reading.reading" ,query-string)))))
    (mapcar (lambda (entry)
              (cdr (assoc "id" entry)))
            query-results)))

;; JMDict entry utility functions

(defun jmdict--primary-reading (entry)
  "Return the primary reading for ENTRY.
The primary reading is the first kanji if any kanji are available
or the first kana reading."
  (or (cdr (assoc "reading" (first (cdr (assoc "Kanji" entry)))))
      (cdr (assoc "reading" (first (cdr (assoc "Reading" entry)))))))

;; JMDict buffer display

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
  (jmdict (first (split-string (button-label button)
                               jmdict--reference-separator))))

(defmacro jmdict--with-jmdict-buffer (buffer &rest body)
  "Evaluate BODY with the JMDict buffer as the current buffer.
BUFFER will be bound to the buffer in BODY. The JMDict buffer
will be placed in `jmdict-mode' and read-only mode will be
inhibited for BODY."
  (declare (indent 1))
  `(let ((,buffer (get-buffer-create jmdict--buffer-name)))
     (with-current-buffer ,buffer
       (jmdict-mode)
       (let ((inhibit-read-only t))
         ,@body))))

(defun jmdict--insert-entry (entry)
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
    (insert (propertize primary 'face 'jmdict-header) "\n")
    (dolist (kanji kanji-readings)
      (insert (propertize (cdr (assoc "reading" kanji))
                          'face 'jmdict-kanji))
      (when-let ((info (mapcar (lambda (i)
                                 (cdr (assoc "info" i)))
                               (cdr (assoc "KanjiInfo" kanji)))))
        (insert " (" (string-join info ", ") ")"))
      (insert "\n"))
    (dolist (kana kana-readings)
      (insert (propertize (cdr (assoc "reading" kana))
                          'face 'jmdict-kana))
      (when-let ((info (mapcar (lambda (i)
                                 (cdr (assoc "info" i)))
                               (cdr (assoc "ReadingInfo" kana))))))
      (when-let ((restrictions
                  (mapcar (lambda (r)
                            (cdr (assoc "restriction" r)))
                          (cdr (assoc "ReadingRestriction" kana)))))
        (insert " (applies only to " (string-join restrictions ", ") ")"))
      (insert "\n")))
  (insert "\n")
  ;; Senses
  (dolist (sense (cdr (assoc "Sense" entry)))
    (when-let ((parts-of-speech
                (mapcar (lambda (p)
                          (cdr (assoc "part_of_speech" p)))
                        (cdr (assoc "SensePartOfSpeech" sense)))))
      (insert (propertize (string-join parts-of-speech "\n")
                          'face 'bold)
              "\n"))
    (when-let ((antonyms
                (mapcar (lambda (a)
                          (cdr (assoc "target" a)))
                        (cdr (assoc "SenseAntonym" sense)))))
      (insert "Antonyms: ")
      (cl-loop for ant on antonyms
               do (insert-button (first ant) 'type 'jmdict-reference)
               unless (cl-endp (cl-rest ant)) do (insert ", "))
      (insert "\n"))
    (when-let ((cross-references
                (mapcar (lambda (c)
                          (cdr (assoc "target" c)))
                        (cdr (assoc "SenseCrossReference" sense)))))
      (insert "See also: ")
      (cl-loop for ref on cross-references
               do (insert-button (first ref) 'type 'jmdict-reference)
               unless (cl-endp (cl-rest ref)) do (insert ", "))
      (insert "\n"))
    (dolist (gloss (cdr (assoc "Gloss" sense)))
      (insert " - " (cdr (assoc "gloss" gloss)) "\n"))
    (insert "\n")))

(defun jmdict (word)
  (interactive "sWord: ")
  (let* ((word-sql (esqlite-format-text word))
         (ids (jmdict--search-entries word))
         (entries (jmdict--get-entries ids)))
    (jmdict--with-jmdict-buffer buffer
      (buffer-disable-undo)
      (erase-buffer)
      (dolist (entry entries)
        (jmdict--insert-entry entry)
        (insert "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (display-buffer buffer))))

(define-derived-mode jmdict-mode special-mode "JMDict"
  "Major mode for JMDict definitions.")

(provide 'jmdict)
;;; jmdict.el ends here
