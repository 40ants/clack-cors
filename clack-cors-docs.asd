(defsystem "clack-cors-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-cors/"
  :class :package-inferred-system
  :description "Provides documentation for clack-cors."
  :source-control (:git "https://github.com/40ants/clack-cors")
  :bug-tracker "https://github.com/40ants/clack-cors/issues"
  :pathname "docs"
  :depends-on ("clack-cors"
               "clack-cors-docs/index"))
