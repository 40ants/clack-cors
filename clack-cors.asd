#-asdf3.1 (error "clack-cors requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "clack-cors"
  :description "A Clack middleware to set CORS related HTTP headers."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-cors/"
  :source-control (:git "https://github.com/40ants/clack-cors")
  :bug-tracker "https://github.com/40ants/clack-cors/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("clack-cors/core")
  :in-order-to ((test-op (test-op "clack-cors-tests"))))


(asdf:register-system-packages "log4cl" (list "LOG"))
