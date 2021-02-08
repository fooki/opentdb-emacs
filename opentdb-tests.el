(require 'opentdb)
(require 'request)

(ert-deftest test-fetch-questions-returns-found-questions ()
  ;; Mockdata returns 2 questions
  (should (= 2 (length (opentdb-fetch-questions)))))


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

    ;; Check mock-request.el for test data
    (should (equal "RW50ZXJ0YWlubWVudDogSmFwYW5lc2UgQW5pbWUgJiBNYW5nYQ==" category))
    (should (equal "bXVsdGlwbGU=" type))
    (should (equal "ZWFzeQ==" difficulty))
    (should (equal "SW4gdGhlIDl0aCBQb2tlbW9uIG1vdmllLCB3aG8gaXMgdGhlIFByaW5jZSBvZiB0aGUgU2VhPw==" quiz))
    (should (equal "TWFuYXBoeQ==" correct-answer))))
