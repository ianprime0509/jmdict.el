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
(require 's)
(require 'subr-x)

(defgroup jmdict nil
  "An interface to the JMDict Japanese dictionary"
  :prefix "jmdict-"
  :group 'applications)

(defvar jmdict--buffer-name "*Jmdict*")

(defvar jmdict-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;; Data types

(cl-defstruct jmdict-jm-entry
  id
  (kanji-readings ())
  (kana-readings ())
  (senses ()))

(cl-defstruct jmdict-jm-kana
  id
  reading
  (restrictions ()))

(cl-defstruct jmdict-jm-sense
  id
  (cross-references ())
  (antonyms ())
  (parts-of-speech ())
  (glosses ()))

(cl-defstruct jmdict-jm-gloss
  id
  gloss
  language
  (gender nil)
  (type nil))

;; SQLite integration

(defconst jmdict--jm-basic-query
  "SELECT
e.id AS entry_id,
  k.id AS kanji_id, k.reading AS kanji,
  r.id AS reading_id, r.reading AS kana,
    rr.id AS restriction_id, rr.restriction,
  s.id AS sense_id,
    scr.id AS cross_reference_id, scr.target AS cross_reference,
    sa.id AS antonym_id, sa.target AS antonym,
    spos.id AS part_of_speech_id, spos.part_of_speech,
      g.id AS gloss_id, g.gloss, g.language AS gloss_language, g.gender AS gloss_gender, g.type AS gloss_type
FROM Entry e
LEFT JOIN Kanji k ON e.id = k.entry_id
JOIN Reading r ON e.id = r.entry_id
  LEFT JOIN ReadingRestriction rr ON r.id = rr.reading_id
JOIN Sense s ON e.id = s.entry_id
  LEFT JOIN SenseCrossReference scr ON s.id = scr.sense_id
  LEFT JOIN SenseAntonym sa ON s.id = sa.sense_id
  LEFT JOIN SensePartOfSpeech spos ON s.id = spos.sense_id
  JOIN Gloss g ON s.id = g.sense_id AND g.gloss IS NOT NULL")

(defun jmdict--get-entries (filter)
  "Find all JMdict entries matching FILTER."
  (let ((entries ()))
    (cl-labels ((process-row
                 (entry-id
                  kanji-id kanji
                  reading-id reading
                  restriction-id restriction
                  sense-id
                  cross-reference-id cross-reference
                  antonym-id antonym
                  part-of-speech-id part-of-speech
                  gloss-id gloss gloss-language gloss-gender gloss-type)
                 (let* ((entry
                         (jmdict--get-or-push entry-id entries
                                              #'jmdict-jm-entry-id
                                              (make-jmdict-jm-entry :id entry-id)))
                        (kana
                         (jmdict--get-or-push reading-id
                                              (jmdict-jm-entry-kana-readings entry)
                                              #'jmdict-jm-kana-id
                                              (make-jmdict-jm-kana :id reading-id
                                                                   :reading reading)))
                        (sense
                         (jmdict--get-or-push sense-id
                                              (jmdict-jm-entry-senses entry)
                                              #'jmdict-jm-sense-id
                                              (make-jmdict-jm-sense :id sense-id)))
                        (gloss
                         (jmdict--get-or-push gloss-id
                                              (jmdict-jm-sense-glosses sense)
                                              #'jmdict-jm-gloss-id
                                              (make-jmdict-jm-gloss
                                               :id gloss-id
                                               :gloss gloss
                                               :language gloss-language
                                               :gender gloss-gender
                                               :type gloss-type))))
                   (when kanji
                     (cl-pushnew kanji (jmdict-jm-entry-kanji-readings entry)
                                 :test #'equal))
                   (when restriction
                     (cl-pushnew restriction (jmdict-jm-kana-restrictions kana)
                                 :test #'equal))
                   (when cross-reference
                     (cl-pushnew cross-reference
                                 (jmdict-jm-sense-cross-references sense)
                                 :test #'equal))
                   (when antonym
                     (cl-pushnew antonym (jmdict-jm-sense-antonyms sense)
                                 :test #'equal))
                   (when part-of-speech
                     (cl-pushnew part-of-speech
                                 (jmdict-jm-sense-parts-of-speech sense)
                                 :test #'equal)))))
      (let ((results
             (nreverse (esqlite-read "jmdict.sqlite3"
                                     (concat jmdict--jm-basic-query
                                             " WHERE " filter
                                             " ORDER BY k.id, r.id, rr.id, s.id, scr.id, sa.id, spos.id, g.id;")))))
        (dolist (row results)
          (apply #'process-row
                 (mapcar (lambda (e) (if (eql e :null) nil e)) row))))
      ;; Fix order of lists in entries
      entries)))

; (jmdict--get-entries "r.reading = 'かな'")

(defmacro jmdict--get-or-push (id place id-function new)
  (let ((var (gensym)))
    `(let ((,var (cl-find ,id ,place :key ,id-function :test #'equal)))
       (if ,var
           ,var
         (push ,new ,place)
         (car ,place)))))

;; JMDict buffer display

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

(defun jmdict--insert-entry (entries)
  (dolist (kanji (jmdict-jm-entry-kanji-readings entry))
    (insert kanji "\n"))
  (newline)
  (dolist (kana (jmdict-jm-entry-kana-readings entry))
    (insert (jmdict-jm-kana-reading kana))
    (when-let ((restrictions (jmdict-jm-kana-restrictions kana)))
      (insert " " (s-join ", " restrictions)))
    (newline))
  (dolist (sense (jmdict-jm-entry-senses entry))
    (when-let ((cross-references (jmdict-jm-sense-cross-references sense)))
      (insert "Cross-references: " (s-join ", " cross-references) "\n"))
    (when-let ((antonyms (jmdict-jm-sense-antonyms sense)))
      (insert "Antonyms: " (s-join ", " antonyms) "\n"))
    (when-let ((parts-of-speech (jmdict-jm-sense-parts-of-speech sense)))
      (insert "Parts of speech: " (s-join ", " parts-of-speech) "\n"))
    (dolist (gloss (jmdict-jm-sense-glosses sense))
      (insert " - "
              (jmdict-jm-gloss-gloss gloss)
              " ("
              (jmdict-jm-gloss-language gloss)
              ")\n"))
    (newline)))

(defun jmdict (word)
  (interactive "sWord: ")
  (let* ((word-sql (esqlite-format-text word))
         (entries (jmdict--get-entries (concat " k.reading = " word-sql
                                               " or r.reading = " word-sql))))
    (jmdict--with-jmdict-buffer buffer
      (erase-buffer)
      (dolist (entry entries)
        (jmdict--insert-entry entry)
        (newline))
      (beginning-of-buffer)
      (display-buffer buffer))))

(define-derived-mode jmdict-mode special-mode "JMDict"
  "Major mode for JMDict definitions.")

(provide 'jmdict)
;;; jmdict.el ends here
