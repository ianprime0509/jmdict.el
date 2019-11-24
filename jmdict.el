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
  k.reading AS kanji,
  r.id AS reading_id, r.reading AS kana,
    rr.restriction,
  s.id AS sense_id,
    scr.target AS cross_reference,
    sa.target AS antonym,
    spos.part_of_speech,
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
  (let ((entries ()))
    (cl-labels ((process-row
                 (entry-id
                  kanji
                  reading-id reading
                  restriction
                  sense-id
                  cross-reference
                  antonym
                  part-of-speech
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
             (esqlite-read "jmdict.sqlite3"
                           (concat jmdict--jm-basic-query " " filter ";"))))
        (dolist (row results)
          (apply #'process-row
                 (mapcar (lambda (e) (if (eql e :null) nil e)) row))))
      entries)))

; (jmdict--get-entries "WHERE r.reading = 'かな'")

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

(jmdict--with-jmdict-buffer buffer
  (erase-buffer)
  (insert "テスト")
  (display-buffer buffer))

(let ((jmdict-buffer (get-buffer-create jmdict--buffer-name)))
  (with-current-buffer jmdict-buffer
    (jmdict-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Test")))
  (display-buffer jmdict-buffer))

(define-derived-mode jmdict-mode special-mode "JMDict"
  "Major mode for JMDict definitions.")

(provide 'jmdict)
;;; jmdict.el ends here
