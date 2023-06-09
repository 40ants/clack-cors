(uiop:define-package #:clack-cors-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:clack-cors-docs/changelog
                #:@changelog)
  (:import-from #:clack-cors
                #:make-cors-middleware
                #:*default-error-response*
                #:*default-allowed-headers*
                #:*default-allowed-methods*
                #:*default-allowed-origin*)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:clack-cors-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "clack-cors-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "clack-cors - A Clack middleware to set CORS related HTTP headers."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "CORS"
                                   ":REQUEST-METHOD"
                                   ":REQUEST-URI"
                                   "Unlicense"
                                   "REPL"
                                   "GIT"))
  (clack-cors system)
  "
[![](https://github-actions.40ants.com/40ants/clack-cors/matrix.svg?only=ci.run-tests)](https://github.com/40ants/clack-cors/actions)

![Quicklisp](http://quickdocs.org/badge/clack-cors.svg)
"
  (@installation section)
  (@usage section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :clack-cors)
```
""")


(defsection @usage (:title "Usage"
                    :ignore-words ("ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"))
  (make-cors-middleware function)
  (*default-allowed-origin* variable)
  (*default-allowed-headers* variable)
  (*default-allowed-methods* variable)
  (*default-error-response* variable))
