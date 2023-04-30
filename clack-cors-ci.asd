(defsystem "clack-cors-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/clack-cors/"
  :class :package-inferred-system
  :description "Provides CI settings for clack-cors."
  :source-control (:git "https://github.com/40ants/clack-cors")
  :bug-tracker "https://github.com/40ants/clack-cors/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "clack-cors-ci/ci"))
