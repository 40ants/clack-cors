(uiop:define-package #:clack-cors
  (:use #:cl)
  (:nicknames #:clack-cors/core)
  (:import-from #:log)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:serapeum
                #:prependf
                #:fmt)
  (:export #:make-cors-middleware))
(in-package #:clack-cors)


(defvar *default-allowed-origin* "*")

(defvar *default-allowed-headers* "Authorization")

(defvar *default-error-response*
  (list 500
        (list :Content-Type "application/json")
        (list "{\"code\": -1, \"message\": \"Unhandled error.\"}")))


(defun value-or-funcall (value env)
  (typecase value
    (function (funcall value) env)
    (t value)))


;; To make it easier to debug:
(declaim (notinline process-cors-middleware))

(defun process-cors-middleware (env app access-control allowed-headers error-response)
  (let* ((response (funcall app env)))
    (cond
      ((length= 3 response)
       (destructuring-bind (code headers content)
           response
         (unless (member :Access-Control-Allow-Origin headers)
           (prependf headers
                     (list :Access-Control-Allow-Origin
                           (value-or-funcall access-control env))))
         (unless (member :Access-Control-Allow-Headers headers)
           (prependf headers
                     (list :Access-Control-Allow-Headers
                           (value-or-funcall allowed-headers env))))
         (list code
               headers
               content)))
      (t
       (log:error "Something strange, I've got response with wrong number of items" response env)
       (value-or-funcall error-response env)))))


(defun make-cors-middleware (app &key
                                   (allowed-origin *default-allowed-origin*)
                                   (allowed-headers *default-allowed-headers*)
                                   (error-response *default-error-response*))
  "Returns a Clack middleware which can be used to add CORS HTTP headers to response.

   By default, it adds:

   - Access-Control-Allow-Origin: *
   - Access-Control-Allow-Headers: Authorization

   But you can pass arguments ALLOWED-ORIGIN and ALLOWED-HEADERS to change this behaviour.

   Also, you can provide a ERROR-RESPONSE argument which will be used as response
   in case if original APP returns response other than a list of three items. This argument
   should be a list like this:

   ```
   (list 500
        (list :Content-Type \"application/json\")
        (list \"{\"code\": -1, \"message\": \"Unhandled error.\"}\"))
   ```

   All arguments can be given as a function of one argument, in this case a function
   will be called with Lack's `env` plist. Most useful keys in this plist are
   :REQUEST-METHOD and :REQUEST-URI.
   "
  (check-type allowed-origin (or string function))
  (check-type allowed-headers (or string function))
  (check-type error-response (or list function))
  
  (flet ((cors-middleware (env)
           (process-cors-middleware env app allowed-origin allowed-headers error-response)))
    #'cors-middleware))
