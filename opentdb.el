(provide 'opentdb)

(require 'cl-lib)
(require 'request)
(require 'json)

(defconst opentdb-api-url "https://opentdb.com/api.php")

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
  (base64-decode-string (cdr (cl-assoc key question))))

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


(cl-defun opentdb-fetch-questions ()
  (let ((result (request
		  opentdb-api-url
		  :params '(("amount" . "50") ("encode" . "base64"))
		  :sync t)))
    (opentdb--json-to-opentdb-questions (request-response-data result))))
