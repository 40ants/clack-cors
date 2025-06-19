(uiop:define-package #:clack-cors
  (:use #:cl)
  (:nicknames #:clack-cors/core)
  (:import-from #:log)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:serapeum
                #:prependf
                #:fmt)
  (:export #:make-cors-middleware
           #:*default-allowed-origin*
           #:*default-allowed-headers*
           #:*default-error-response*
           #:*default-allowed-methods*))
(in-package #:clack-cors)


(defvar *default-allowed-origin* nil
  "Default value to return as `Access-Control-Allow-Origin` HTTP header.")

(defvar *default-allowed-headers* nil
  "Default value to return as `Access-Control-Allow-Headers` HTTP header.")

(defvar *default-allowed-methods* nil
  "Default value to return as `Access-Control-Allow-Methods` HTTP header.")

(defvar *access-control-allow-credentials* nil
  "Default value to return as `Access-Control-Allow-Credentials` HTTP header.")  

(defvar *default-error-response*
  (list 500
        (list :Content-Type "application/json")
        (list "{\"code\": -1, \"message\": \"Unhandled error.\"}"))
  "Default value to return if main app will not return a list of three items.")


(defun value-or-funcall (value env response-headers)
  (typecase value
    (function (funcall value env response-headers))
    (t value)))


;; To make it easier to debug:
(declaim (notinline process-cors-middleware))

(defun process-cors-middleware (env app access-control allowed-headers allowed-methods allowed-credentials error-response)
  (let* ((response (funcall app env)))
    (cond
      ((length= 3 response)
       (destructuring-bind (code headers content)
           response
         (setf (getf headers :Access-Control-Allow-Origin)
               (or (value-or-funcall access-control env headers)
                   (getf headers :Access-Control-Allow-Origin)))
         (setf (getf headers :Access-Control-Allow-Headers)
               (or (value-or-funcall allowed-headers env headers)
                   (getf headers :Access-Control-Allow-Headers)))
         (setf (getf headers :Access-Control-Allow-Methods)
               (or (value-or-funcall allowed-methods env headers)
                   (getf headers :Access-Control-Allow-Methods)))
         (setf (getf headers :Access-Control-Allow-Credentials)
               (or (value-or-funcall allowed-credentials env headers)
                   (getf headers :Access-Control-Allow-Credentials)))          
         (list code
               headers
               content)))
      (t
       (log:error "Something strange, I've got response with wrong number of items" response env)
       (value-or-funcall error-response env nil)))))


(defun make-cors-middleware (app &key
                                 (allowed-origin *default-allowed-origin*)
                                 (allowed-headers *default-allowed-headers*)
                                 (allowed-methods *default-allowed-methods*)
                                 (allowed-credentials *access-control-allow-credentials*)
                                 (error-response *default-error-response*))
  "Returns a Clack middleware which can be used to override CORS HTTP headers in response.

   You can pass arguments ALLOWED-ORIGIN, ALLOWED-HEADERS,ALLOWED-CREDENTIALS and ALLOWED-METHODS to override corresponding headers.

   If given, these arguments are extend headers, returned by the main application. For example, if main application
   already returns `Access-Control-Allow-Headers` with value `Content-Type`, then it will be overwritten with
   the value `Authorization`. To implement a smarter logic, pass as an argument a function of two variables - initial
   `env` plist and resulting headers `plist`. The function should return a string which will be used
   to replace a header value.

   Also, you can provide a ERROR-RESPONSE argument which will be used as response
   in case if original APP returns response other than a list of three items. This argument
   should be a list like this:

   ```
   (list 500
        (list :Content-Type \"application/json\")
        (list \"{\\\"code\\\": -1, \\\"message\\\": \\\"Unhandled error.\\\"}\"))
   ```

   All arguments can be given as a function of two argument, in this case a function
   will be called with Lack's `env` plist and a plist of headers returned by the main application.
   Most useful keys in the `env` plist are :REQUEST-METHOD and :REQUEST-URI.
   "
  (check-type allowed-origin (or string function))
  (check-type allowed-headers (or string function))
  (check-type allowed-methods (or string function))
  (check-type allowed-credentials (or string function))
  (check-type error-response (or list function))
  
  (flet ((cors-middleware (env)
           (process-cors-middleware env app
                                    allowed-origin
                                    allowed-headers
                                    allowed-methods
                                    allowed-credentials
                                    error-response)))
    #'cors-middleware))
