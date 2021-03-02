(provide 'request)

(defvar request-url nil)
(defvar request-params nil)
(defvar mocked-request-response-status-code 200)

(defun request (url &key params &key sync)
  (progn
    (setq request-url url)
    (setq request-params params)
    'result))

(defun request-response-status-code (result)
  mocked-request-response-status-code)

(defun request-response-data (result)
"
{
  \"response_code\": 0,
  \"results\": [
    {
      \"category\": \"RW50ZXJ0YWlubWVudDogSmFwYW5lc2UgQW5pbWUgJiBNYW5nYQ==\",
      \"type\": \"bXVsdGlwbGU=\",
      \"difficulty\": \"ZWFzeQ==\",
      \"question\": \"SW4gdGhlIDl0aCBQb2tlbW9uIG1vdmllLCB3aG8gaXMgdGhlIFByaW5jZSBvZiB0aGUgU2VhPw==\",
      \"correct_answer\": \"TWFuYXBoeQ==\",
      \"incorrect_answers\": [
        \"QXNo\",
        \"TWF5\",
        \"UGhhbnRvbQ==\"
      ]
    },
    {
      \"category\": \"RW50ZXJ0YWlubWVudDogVmlkZW8gR2FtZXM=\",
      \"type\": \"bXVsdGlwbGU=\",
      \"difficulty\": \"bWVkaXVt\",
      \"question\": \"V2hhdCB0eXBlIG9mIGdlbnJlIGlzIHRoZSBjb250cm92ZXJzaWFsIDIwMTUgZ2FtZSAiSGF0cmVkIj8=\",
      \"correct_answer\": \"U2hvb3QgJ0VtIFVw\",
      \"incorrect_answers\": [
        \"UG9pbnQgJiBDbGljaw==\",
        \"TU1PUlBH\",
        \"U2ltdWxhdGlvbg==\"
      ]
    }
  ]
}
")
