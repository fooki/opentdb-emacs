(require 'opentdb)
(require 'request)

;;;
;;; All tests uses the mock request defined in mock-request.el
;;;
(ert-deftest test-fetch-questions-returns-found-questions ()
  ;; Mockdata returns 2 questions
  (should (= 2 (length (opentdb-fetch-questions)))))

(ert-deftest test-fetch-questions-uses-opentdb-url ()
  (opentdb-fetch-questions)
  (should (equal "https://opentdb.com/api.php" request-url)))

(ert-deftest test-fetch-questions-allows-provided-amount-and-category ()
  (opentdb-fetch-questions
   :amount 3 :difficulty "easy" :category 5 :type "boolEAN")
  (should
   (equal '(("amount" . 3)
	    ("category" . 5)
	    ("difficulty" . "easy")
	    ("type" . "boolean")
	    ("encode" . "base64"))
	  request-params)))

(ert-deftest test-fetch-questions-uses-custom-variables ()
  (setq opentdb-category 666)
  (setq opentdb-difficulty "Hard")
  (setq opentdb-type "muLTIPLe")
  (opentdb-fetch-questions :amount 3)
  (should
   (equal '(("amount" . 3)
	    ("category" . 666)
	    ("difficulty" . "hard")
	    ("type" . "multiple")
	    ("encode" . "base64"))
	  request-params)))

(ert-deftest test-fetch-questions-defaults-to-1-question-with-empty-options ()
  (opentdb-fetch-questions)
  (should
   (equal '(("amount" . 1)
	    ("category" . "")
	    ("difficulty" . "")
	    ("type" . "")
	    ("encode" . "base64"))
	  request-params)))

(ert-deftest test-fetch-questions-are-set-correctly ()
  (let* ((question (nth 0 (opentdb-fetch-questions)))
	 (category
	  (base64-encode-string (opentdb-question-category question)))
	 (type
	  (base64-encode-string (opentdb-question-type question)))
	 (difficulty
	  (base64-encode-string (opentdb-question-difficulty question)))
	 (quiz
	  (base64-encode-string (opentdb-question-question question)))
	 (correct-answer
	  (base64-encode-string (opentdb-question-correct-answer question))))

    ;; Check mock-request.el for base64 encoded test data
    (should (equal "RW50ZXJ0YWlubWVudDogSmFwYW5lc2UgQW5pbWUgJiBNYW5nYQ==" category))
    (should (equal "bXVsdGlwbGU=" type))
    (should (equal "ZWFzeQ==" difficulty))
    (should (equal "SW4gdGhlIDl0aCBQb2tlbW9uIG1vdmllLCB3aG8gaXMgdGhlIFByaW5jZSBvZiB0aGUgU2VhPw==" quiz))
    (should (equal "TWFuYXBoeQ==" correct-answer))))

(ert-deftest test-next-question-does-not-crash ()
  (opentdb-next-question))


(ert-deftest test-prefixing-answers-adds-letters-to-the-answers ()
    (let* ((question (make-opentdb-question
		     :question "yadayada yada?"
		     :category "sports"
		     :type "multiple"
		     :difficulty "easy"
		     :correct-answer "beets"
		     :answers (list "bears" "beets" "Battlestar Galactica")))
	   (prefixed-answers (opentdb--prefix-answers question))
	   (prefixed-correct (opentdb--prefix-correct-answer question)))
      (should (equal '((A . "bears") (B . "beets") (C . "Battlestar Galactica")) prefixed-answers))

      ;; The correct answer is prefixed with B instead of A since its the second
      ;; answer.
      (should (equal (cons 'B "beets") prefixed-correct))))

(ert-deftest test-raises-error-when-fetching-questions-fail ()
  ;; assert it worked before trying errors
  (setq mocked-request-response-status-code 200)
  (opentdb-fetch-questions)

  ;; nil means we couldnt even contact the web server
  (setq mocked-request-response-status-code nil)
  (should-error (opentdb-fetch-questions))

  (setq mocked-request-response-status-code 404)
  (should-error (opentdb-fetch-questions))

  ;; assert it works again
  (setq mocked-request-response-status-code 200)
  (opentdb-fetch-questions))
