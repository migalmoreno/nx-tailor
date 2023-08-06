(defsystem #:nx-tailor
  :description "nx-tailor is a theme manager for Nyxt."
  :author "Miguel Ángel Moreno"
  :homepage "https://github.com/migalmoreno/nx-tailor"
  :license "BSD 3-Clause"
  :version "0.2.0"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "tailor")))
