(uiop:define-package #:clack-cors-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:clack-cors-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.1 2023-05-28
         "* Fix error in value-or-funcall function.")
  (0.2.0 2023-05-11
         "* Now given header values are always replace original headers returned by the main application.

            This change was made to give more control on how headers are returned.
          * Another change is that now functions given instead of literal values, should accept two arguments
            instead of one.

            First argument is an `env` plist describing the request and second argument is
            `response-headers` plist, returned by the main app. Using both of them, user or the library
            can have more flexible control on how these headers should be produced.

          * Also, the library now supports additional header `Access-Control-Allow-Methods`.
            ")
  (0.1.0 2023-02-05
         "* Initial version."))
