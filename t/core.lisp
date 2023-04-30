(uiop:define-package #:clack-cors-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing)
  (:import-from #:clack.test
                #:testing-app)
  (:import-from #:clack.test
                #:localhost))
(in-package #:clack-cors-tests/core)


;; For some reason, this test fails
;; on OSX, because testing-app internally cant
;; destroy-thread with hunchentoot-handler:
(deftest test-example ()
  (testing-app "defaults"
      (lambda (env)
        (declare (ignore env))
        '(200 (:content-type "text/plain")
          ("Hello" "World")))
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (ok (eql status 200))
      (ok (equal body "HelloWorld")))))
