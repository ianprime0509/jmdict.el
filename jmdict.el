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
(require 'esqlite)
(require 'subr-x)

(defgroup jmdict nil
  "An interface to the JMDict Japanese dictionary"
  :prefix "jmdict-"
  :group 'applications)

(defcustom jmdict-jmdict-path
  (expand-file-name "~/jmdict/jmdict.sqlite3")
  "Path to the JMDict SQLite database."
  :group 'jmdict
  :type 'file
  :set (lambda (symbol path)
         (set-default symbol (expand-file-name path))))

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

;; Utility macros

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
parent (ignored at the top level). The first element of COLUMNS
may be :optional to indicate that a left join should be used
instead of an inner join, if applicable.

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
    ;; Ignore keyword options in columns
    (setf columns (cl-remove-if #'keywordp columns))
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
                 (setf parsed (cl-acons column value parsed)))
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
        (setf parsed (cl-acons (first sub-spec) sub-value parsed))))
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
           (cl-loop for (table join-on optional) in (cl-rest tables)
                    collect (format "%s %s ON %s"
                                    (if optional "LEFT JOIN" "JOIN")
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
format. Unary and binary operators are converted in the natural
way; expressions with more than two operands are converted to
multiple applications of a binary operator from left to right."
  (if (not (listp expr)) (format "%s" expr)
    (cl-destructuring-bind (op &rest operands) expr
      (case (length operands)
        (0 (error "No operands provided"))
        (1 (format "(%s %s)"
                   op
                   (jmdict--convert-sql-expression (first operands))))
        (2 (format "(%s %s %s)"
                   (jmdict--convert-sql-expression (first operands))
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
element in columns is of the form (TABLE COLUMN).

PARENT is the name of the parent table, or nil if there is none."
  (cl-destructuring-bind (table parent-id columns &rest sub-specs)
      spec
    (let ((tables
           (list (list table
                       (when parent
                         (format "%s.%s = %s.id" table parent-id parent))
                       (eql :optional (first columns)))))
          (columns
           (nconc (list (list table "id"))
                  (mapcar (lambda (column) (list table column))
                          (cl-remove-if #'keywordp columns)))))
      (dolist (sub-spec sub-specs)
        (cl-multiple-value-bind (sub-tables sub-columns)
            (jmdict--parse-query-spec sub-spec table)
          (setf tables (nconc tables sub-tables))
          (setf columns (nconc columns sub-columns))))
      (cl-values tables columns))))

(jmdict--defun-cached jmdict--get-entries (ids)
  "Find all JMdict entries with IDs in IDS."
  (jmdict--query
   jmdict-jmdict-path
   '("Entry" nil ()
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
   `(and (in "Entry.id" ,(format "(%s)" (string-join ids ", ")))
         (= "Gloss.language" "'eng'"))))

(jmdict--defun-cached jmdict--search-entries (query)
  "Search for JMDict entries matching QUERY.
The return value is a list of entry IDs."
  (let* ((is-japanese (jmdict--is-japanese query))
         (query-string (esqlite-format-text query))
         (kanji-results
          (when is-japanese
            (jmdict--query
             jmdict-jmdict-path
             '("Entry" nil ("id")
               ("Kanji" "entry_id" ()))
             `(= "Kanji.reading" ,query-string))))
         (kana-results
          (when is-japanese
            (jmdict--query
             jmdict-jmdict-path
             '("Entry" nil ("id")
               ("Reading" "entry_id" ()))
             `(= "Reading.reading" ,query-string))))
         (gloss-results
          (unless is-japanese
            (jmdict--query
             jmdict-jmdict-path
             '("Entry" nil ("id")
               ("Sense" "entry_id" ()
                ("Gloss" "sense_id" ())))
             `(and (like "Gloss.gloss" ,query-string)
                   (= "Gloss.language" "'eng'")))))
         (all-results
          (append kanji-results kana-results gloss-results)))
    (seq-uniq (mapcar (lambda (entry)
                        (cdr (assoc "id" entry)))
                      all-results))))

;; Utility functions

(defun jmdict--is-japanese (query)
  "Return non-nil if QUERY is Japanese text."
  (cl-loop for char across query
           thereis (seq-contains '(kana han cjk-misc)
                                 (aref char-script-table char))))

(defun jmdict--primary-reading (entry)
  "Return the primary reading for ENTRY.
The primary reading is the first kanji if any kanji are available
or the first kana reading."
  (or (cdr (assoc "reading" (first (cdr (assoc "Kanji" entry)))))
      (cdr (assoc "reading" (first (cdr (assoc "Reading" entry)))))))

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
                        (jmdict--values (first path) alist multiple)
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
                          'face 'jmdict-kanji))
      (when-let ((info (jmdict--values '("KanjiInfo" "info") kanji)))
        (insert " (" (string-join info ", ") ")"))
      (insert "\n"))
    (dolist (kana kana-readings)
      (insert (propertize (cdr (assoc "reading" kana))
                          'face 'jmdict-kana))
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
               do (insert-button (first ant) 'type 'jmdict-reference)
               unless (cl-endp (cl-rest ant)) do (insert ", "))
      (insert "\n"))
    (when-let ((cross-references
                (jmdict--values '("SenseCrossReference" "target") sense)))
      (insert "See also: ")
      (cl-loop for ref on cross-references
               do (insert-button (first ref) 'type 'jmdict-reference)
               unless (cl-endp (cl-rest ref)) do (insert ", "))
      (insert "\n"))
    (dolist (gloss (jmdict--values "Gloss" sense))
      (insert " - " (cdr (assoc "gloss" gloss)) "\n"))
    (insert "\n")))

(defmacro jmdict--with-jmdict-buffer (buffer &rest body)
  "Evaluate BODY with the JMDict buffer as the current buffer.
BUFFER will be bound to the buffer in BODY. The JMDict buffer
will be placed in `jmdict-mode' and read-only mode will be
inhibited for BODY."
  (declare (indent 1))
  `(let ((,buffer (get-buffer jmdict--buffer-name)))
     (unless ,buffer
       (setf ,buffer (get-buffer-create jmdict--buffer-name))
       (with-current-buffer ,buffer
         (jmdict-mode)))
     (with-current-buffer ,buffer
       (let ((inhibit-read-only t))
         ,@body)
       (set-buffer-modified-p nil))))

;; Interactive commands

(defvar jmdict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'jmdict)
    (define-key map (kbd "l") #'jmdict-go-back)
    (define-key map (kbd "r") #'jmdict-go-forward)
    (define-key map (kbd "n") #'jmdict-next-entry)
    (define-key map (kbd "p") #'jmdict-previous-entry)
    map))

(defvar-local jmdict--history-back-stack ()
  "The stack of previous history entries.
Each element on the stack is of the form (QUERY . POSITION),
where QUERY is the query associated with the buffer and POSITION
is the position of point.")

(defvar-local jmdict--history-forward-stack ()
  "The stack of next history entries.
Each element on the stack is of the same form as
`jmdict--history-back-stack'.")

(defvar-local jmdict--current-query nil
  "The query associated with the current buffer.")

(defun jmdict--display-query-result (query)
  "Find and display all entries matching QUERY.
Clear the current buffer, insert the formatted entries matching
QUERY and navigate to the beginning of the buffer. Update the
history stacks as appropriate."
  (let ((inhibit-read-only t))
    (let* ((ids (jmdict--search-entries query))
           (entries (jmdict--get-entries ids)))
      (erase-buffer)
      (dolist (entry entries)
        (jmdict--insert-entry entry)
        (insert "\n"))
      (goto-char (point-min)))))

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

(defun jmdict-go-back ()
  "Go back to the results of the previous query."
  (interactive)
  (if-let ((back (pop jmdict--history-back-stack)))
      (progn
        (when jmdict--current-query
          (push (cons jmdict--current-query (point))
                jmdict--history-forward-stack))
        (setf jmdict--current-query (car back))
        (jmdict--display-query-result (car back))
        (goto-char (cdr back))
        (recenter))
    (message "No previous query")))

(defun jmdict-go-forward ()
  "Go forward to the results of the next query.
This undoes the action of `jmdict-go-back'."
  (interactive)
  (if-let ((forward (pop jmdict--history-forward-stack)))
      (progn
        (when jmdict--current-query
          (push (cons jmdict--current-query (point))
                jmdict--history-back-stack))
        (setf jmdict--current-query (car forward))
        (jmdict--display-query-result (car forward))
        (goto-char (cdr forward))
        (recenter))
    (message "No next query")))

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
  (interactive "sQuery: ")
  (jmdict--with-jmdict-buffer buffer
    (when jmdict--current-query
      (push (cons jmdict--current-query (point))
            jmdict--history-back-stack))
    (setf jmdict--history-forward-stack ())
    (setf jmdict--current-query query)
    (jmdict--display-query-result query)
    (display-buffer buffer)))

(define-derived-mode jmdict-mode special-mode "JMDict"
  "Major mode for JMDict definitions."
  (buffer-disable-undo))

(provide 'jmdict)
;;; jmdict.el ends here
