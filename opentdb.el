(provide 'opentdb)

(require 'cl-lib)
(require 'request)
(require 'json)

(defgroup opentdb nil
  "Emacs quiz based on the opentdb api"
  :group 'games)

(defcustom opentdb-category nil
  "nil for any category, otherwise number 9 - 32. See opentdb-categories"
  :group 'opentdb
  :type 'string)

(defcustom opentdb-difficulty nil
  "nil for any difficulty, otherwise easy medium or hard"
  :group 'opentdb
  :type 'string)

(defconst opentdb-api-url "https://opentdb.com/api.php")

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


(cl-defun opentdb-fetch-questions
    (&key (amount 1) &key (category opentdb-category) &key (difficulty opentdb-difficulty))
  (let* ((nil-safe-category (if (null category) "" category))
	 (nil-safe-difficulty (downcase (if (null difficulty) "" difficulty)))
	 (result (request
		   opentdb-api-url
		   :params `(("amount" . ,amount)
			     ("category" . ,nil-safe-category)
			     ("difficulty" . ,nil-safe-difficulty)
			     ("encode" . "base64"))
		   :sync t)))
    (opentdb--json-to-opentdb-questions (request-response-data result))))
