(uiop:define-package #:nx-tailor
  (:nicknames #:tailor)
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:user-class
                #:define-mode
                #:define-command
                #:define-command-global
                #:*browser*
                #:theme
                #:current-buffer
                #:current-window
                #:find-submode
                #:url)
  (:documentation "An interface to manage themes in Nyxt."))
