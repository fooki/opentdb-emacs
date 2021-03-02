;;; opentdb.el --- A package for using the opentdb api -*- lexical-binding: t -*-

;; Copyright (C) 2021 Karl Johansson

;; Author: Karl Johansson <karljoh85@gmail.com>
;; URL: https://github.com/fooki/opentdb-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (request "0.3.2"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commands:
;; opentdb-next-question - Opens up a new buffer and runs a quiz.

;;; Commentary:

;; Build and run quizzes with the help of opentdb. You can either build your own
;; quizes using (opentdb-fetch-questions) or use the provided GUI with
;; (opentdb-next-question)

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'json)
(require 'widget)

(defgroup opentdb nil
  "Emacs quiz based on the opentdb api"
  :group 'games)

(defcustom opentdb-category nil
  "Nil for any category, otherwise number 9 - 32. See `opentdb-categories'."
  :group 'opentdb
  :type 'string)

(defcustom opentdb-difficulty nil
  "Nil for any difficulty, otherwise easy, medium or hard."
  :group 'opentdb
  :type 'string)

(defcustom opentdb-type nil
  "Nil for any type, otherwise boolean or multiple."
  :group 'opentdb
  :type 'string)

(defconst opentdb-api-url "https://opentdb.com/api.php")

;; This const isn't really used and might be outdated at any time.
(defconst opentdb-categories
  '(("9" .  "General Knowledge")
    ("10" . "Entertainment: Books")
    ("11" . "Entertainment: Film")
    ("12" . "Entertainment: Music")
    ("13" . "Entertainment: Musicals & Theatres")
    ("14" . "Entertainment: Television")
    ("15" . "Entertainment: Video Games")
    ("16" . "Entertainment: Board Games")
    ("17" . "Science & Nature")
    ("18" . "Science: Computers")
    ("19" . "Science: Mathematics")
    ("20" . "Mythology")
    ("21" . "Sports")
    ("22" . "Geography")
    ("23" . "History")
    ("24" . "Politics")
    ("25" . "Art")
    ("26" . "Celebrities")
    ("27" . "Animals")
    ("28" . "Vehicles")
    ("29" . "Entertainment: Comics")
    ("30" . "Science: Gadgets")
    ("31" . "Entertainment: Japanese Anime & Manga")
    ("32" . "Entertainment: Cartoon & Animations")))

(defconst opentdb--correct-answer-shown
  nil
  "Helps keep track of state in the quiz GUI.")

(cl-defstruct opentdb-question
  question
  category
  difficulty
  type
  answers
  correct-answer)

;; Stolen from rosetta code: Knuth_shuffle
(cl-defun opentdb--nshuffle (sequence)
  (cl-loop
   for i from (length sequence) downto 2
   do (cl-rotatef (elt sequence (random i))
		  (elt sequence (1- i))))
  sequence)

(cl-defun opentdb--vector->list (vec)
  (cl-map 'list #'identity vec))

(cl-defun opentdb--extract-b64-val (key question)
  ;; decodes a single base64 field in the provided question
  (base64-decode-string (cdr (cl-assoc key question))))

;; ((string . base64-string) ...) -> opentdb-question)
(cl-defun opentdb--assoc-question->opentdb-question (question)
  (let ((correct (opentdb--extract-b64-val 'correct_answer question))
	(incorrect (opentdb--vector->list
		    (cl-map 'list
			    #'base64-decode-string
			    (cdr (cl-assoc 'incorrect_answers question))))))
    (make-opentdb-question
     :question (opentdb--extract-b64-val 'question question)
     :category (opentdb--extract-b64-val 'category question)
     :type (opentdb--extract-b64-val 'type question)
     :difficulty (opentdb--extract-b64-val 'difficulty question)
     :correct-answer correct
     :answers (opentdb--nshuffle (append incorrect (list correct))))))

(cl-defun opentdb--json-to-opentdb-questions (json-string)
  (let* ((parsed (json-read-from-string json-string))
	 (questions (append (cdr (nth 1 parsed)) nil)))
    (cl-loop
     for question in questions
     collect (opentdb--assoc-question->opentdb-question question))))


(cl-defun opentdb-fetch-questions
    (&key (amount 1)
	  &key (category opentdb-category)
	  &key (type opentdb-type)
	  &key (difficulty opentdb-difficulty))
  "Fetches questions from the opentdb api and returns (list opentdb-question ...)"


  (let* ((nil-safe-category (if (null category) "" category))
	 (nil-safe-difficulty (downcase (if (null difficulty) "" difficulty)))
	 (nil-safe-type (downcase (if (null type) "" type)))

	 ;; TODO: We should add error handling.
	 (result (request
		   opentdb-api-url
		   :params `(("amount" . ,amount)
			     ("category" . ,nil-safe-category)
			     ("difficulty" . ,nil-safe-difficulty)
			     ("type" . ,nil-safe-type)
			     ("encode" . "base64"))
		   :sync t)))
    (opentdb--json-to-opentdb-questions (request-response-data result))))

(defconst opentdb--answer-letter-index
  ;; Here we assume that no questions have more than 26 choices. If that is
  ;; incorrect then I owe you a beer.
  '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(cl-defun opentdb--prefix-answers (question)
  ;; Prefixes all answers with a letter to make it easier to refer to
  ;; them. Example "I think answer B is correct!".
  (cl-mapcar #'cons
	     opentdb--answer-letter-index
	     (opentdb-question-answers question)))

(cl-defun opentdb--prefix-correct-answer (question)
  ;; Prefixes the correct answer with a letter, giving it the same letter as it
  ;; has in the list of all answers.
  (let* ((correct-answer (opentdb-question-correct-answer question))
	 (answers (opentdb-question-answers question))
  	 (correct-pos (cl-position correct-answer answers :test 'equal))
  	 (prefix (nth correct-pos opentdb--answer-letter-index)))
    (cons prefix correct-answer)))

(cl-defun opentdb--next-question-button ()
  ;; Inserts a button widget at current point for starting over the quiz.
  (widget-create
   'push-button
   :notify '(lambda (&rest ignore) (opentdb-next-question))
   "Next question"))

(cl-defun opentdb--prefixed-answer-list (question)
  ;; Inserts a bunch of widgets at current point containing all the answers for
  ;; the provided question.
  (let ((prefixed-answers (opentdb--prefix-answers question)))
    (cl-map
     'list
     (lambda (el)
       (progn
	 (widget-insert (format "%s - %s" (car el) (cdr el)))
	 (widget-insert "\n\n")))
     prefixed-answers)))

(cl-defun opentdb--question-banner (question)
  (widget-insert (format "\n\n### %s ###\n\n" (opentdb-question-question question))))

(cl-defun opentdb--margin-widget ()
  (widget-insert "\n\n"))

(cl-defun opentdb--show-answer-button (question)
  (widget-create
   'push-button
   :value "Show answer"

   ;; When pushed, the button will do two things:
   ;; 1. reveal the answer to the question
   ;; 2. inserting a button for showing another question.
   :notify (lambda (widget &rest _ignore)
	      (let ((prefixed-correct-answer (opentdb--prefix-correct-answer question)))
		;; Since this button is still shown after pressed,
		;; we should only react on the first button press.
		(when (not opentdb--correct-answer-shown)
		  (progn
		    (widget-value-set
		     widget
		     (format "Correct answer: %s - %s"
			     (car prefixed-correct-answer)
			     (cdr prefixed-correct-answer)))
		    (goto-char (point-max))
		    (widget-insert "\n\n")
		    (opentdb--next-question-button)

		    ;; A hack to keep track of whether we have
		    ;; clicked the show answer button already.
		    (setq opentdb--correct-answer-shown t)

		    (widget-setup)))))))


(defun opentdb-next-question ()
  "Show a random question in a new buffer."
  (interactive)

  ;; For now we only fetch a single question each time we run this.
  (let* ((question (nth 0 (opentdb-fetch-questions))))

    ;; Clean up a potentially old buffer
    (kill-buffer (get-buffer-create "*OpenTDB*"))
    (switch-to-buffer "*OpenTDB*")

    ;; A hack to keep track of whether we have clicked the show answer button
    ;; already.
    (setq opentdb--correct-answer-shown nil)

    (opentdb--question-banner question)
    (opentdb--margin-widget)
    (opentdb--prefixed-answer-list question)
    (opentdb--margin-widget)
    (opentdb--show-answer-button question)

    (use-local-map widget-keymap)
    (widget-setup)))

(provide 'opentdb)
;;; opentdb.el ends here
