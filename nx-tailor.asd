(defsystem #:nx-tailor
  :description "An interface to manage themes in Nyxt."
  :author "conses"
  :homepage "https://git.sr.ht/~conses/nx-tailor"
  :license "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "tailor")))
