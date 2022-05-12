(in-package #:nx-tailor)
(nyxt:use-nyxt-package-nicknames)

(defparameter *current-theme* nil
  "Current `internal-theme'.")

(sera:export-always 'make-theme)
(defun make-theme (name &rest extra-slots &key &allow-other-keys)
  "Builds a `nx-tailor' theme. NAME is required and EXTRA-SLOTS
can vary depending on the theme complexity."
  (apply #'make-instance 'internal-theme :name name extra-slots))

(defun list-of-lists-p (object)
  "Returns non-nil of OBJECT consists of a list of lists."
  (and (listp object)
       (every #'listp object)))

(define-class cut ()
  ((name
    :type (or null list)
    :documentation "The name of the cut.")
   (user-buffer
    :type (or null list)
    :documentation "A list of CSS rules to tweak a user buffer.")
   (prompt
    :type (or null list)
    :documentation "A list of CSS rules to tweak the prompt buffer.")
   (status
    :type (or null list)
    :documentation "A list of CSS rules to tweak the status buffer.")
   (message
    :type (or null list)
    :documentation "A list of CSS rules to tweak the message area.")
   (hint
    :type (or null list)
    :documentation "A list of CSS rules to tweak the look of hints."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-slot-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A cut is a theme's custom finishing which styles various bits of Nyxt's interface.")
  (:metaclass user-class))

(define-class internal-theme (theme:theme)
  ((name
    ""
    :type string
    :documentation "The name of the theme.")
   (cut
    nil
    :type (or null cut)
    :documentation "`cut' provides styling for interface parts and allows to dynamically
change themes within a browser session."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class theme-source (prompter:source)
  ((prompter:name "User themes")
   (prompter:constructor (themes (current-tailor-mode)))
   (prompter:active-attributes-keys '("Name"))))

(defun get-original-style (element &optional style-slot parent-class)
  "Finds the original STYLE-SLOT slot value of ELEMENT. If PARENT-CLASS,
looks through all the children class slots."
  (sb-mop:slot-definition-initform
   (find (or style-slot 'nyxt:style)
         (if parent-class
             (sb-mop:class-slots
              (find-class element))
             (sb-mop:class-direct-slots
              (find-class element)))
         :key (lambda (el)
                (slot-value el 'sb-pcl::name)))))

(defun theme-handler (buffer mode)
  "Handler function to re-calculate styles in BUFFER with MODE."
  (setf (nyxt::style buffer) (compute-style
                              *current-theme*
                              :element 'nyxt:buffer
                              :accessor #'user-buffer))
  (when (find-submode (resolve-symbol :web-mode :mode) buffer)
    (setf (nyxt/web-mode:box-style (find-submode (resolve-symbol :web-mode :mode) buffer))
          (compute-style *current-theme*
                         :element 'nyxt/web-mode:web-mode
                         :style-slot 'nyxt/web-mode:box-style
                         :accessor #'hint))))

(define-mode tailor-mode (nyxt/style-mode:style-mode)
  "Mode that manages custom browser themes."
  ((themes
    '()
    :type list
    :documentation "`internal-theme' objects among which to select the main internal interface theme.")
   (auto-p
    nil
    :type boolean
    :documentation "Whether to automatically apply a `theme' variant based on the system environment.")))

(defmethod nyxt:enable ((mode tailor-mode) &key)
  (with-slots (auto-p) mode
    (unless (or (not (themes mode))
                (find (theme *browser*) (themes mode) :test #'equal)
                *current-theme*)
      (or (and auto-p
               (if (string= (uiop:getenv "GTK_THEME") ":light")
                   (select-theme (name (find-theme-variant mode)) mode)
                   (setf (nyxt::style (buffer mode))
                         (compute-style
                          (select-theme
                           (name (find-theme-variant mode :dark t))
                           mode)
                          :element 'nyxt:buffer
                          :accessor #'user-buffer))))
          (select-theme (name (car (themes mode))) mode))
      (hooks:add-hook (nyxt:buffer-before-make-hook *browser*)
                      (make-instance
                       'hooks:handler
                       :fn (lambda (buffer)
                             (theme-handler buffer mode))
                       :name 'handle-theme)))))

(defmethod nyxt:disable ((mode tailor-mode) &key)
  (hooks:remove-hook (nyxt:buffer-before-make-hook *browser*) #'theme-handler)
  (hooks:remove-hook (nyxt:prompt-buffer-make-hook *browser*) 'style-prompt-buffer)
  (setf *current-theme* nil))

(defun find-theme-variant (mode &key dark)
  "Finds the first light theme variant from MODE. If DARK, it finds the first dark theme."
  (if dark
      (find-if #'theme:dark-p (themes mode))
      (find-if-not #'theme:dark-p (themes mode))))

(defun compute-style (theme &key element accessor (style-slot nil))
  (str:concat
   (eval (get-original-style element (or style-slot)))
   (and (cut theme) (funcall (eval `(lambda (theme)
                                      (theme:themed-css theme
                                        ,@(funcall accessor (cut theme)))))
                             theme))))

(defun current-tailor-mode ()
  "Returns `tailor-mode' if it's active in the current buffer."
  (find-submode
   (resolve-symbol :tailor-mode :mode '(:nx-tailor))))

(defun reload-window-style ()
  "Reloads the window style."
  (if (not (current-window))
      (hooks:add-hook (nyxt:window-make-hook *browser*)
                      (make-instance
                       'hooks:handler
                       :fn (lambda (window)
                             (setf (nyxt::style (nyxt::status-buffer window))
                                   (compute-style *current-theme*
                                                  :element 'nyxt:status-buffer
                                                  :accessor #'status)
                                   (nyxt:message-buffer-style window)
                                   (compute-style *current-theme*
                                                  :element 'nyxt:window
                                                  :style-slot 'nyxt:message-buffer-style
                                                  :accessor #'message))
                             (hooks:remove-hook (nyxt:window-make-hook *browser*)
                                                'style-window-on-startup))
                       :name 'style-window-on-startup))
      (setf (nyxt::style (nyxt::status-buffer (current-window)))
            (compute-style *current-theme*
                           :element 'nyxt:status-buffer
                           :accessor #'status)
            (nyxt:message-buffer-style (current-window))
            (compute-style *current-theme*
                           :element 'nyxt:window
                           :style-slot 'nyxt:message-buffer-style
                           :accessor #'message)))
  (nyxt::print-status)
  (nyxt::echo ""))

(defun reload-prompt-style ()
  "Reloads the prompt buffer style."
  (if (not (cut *current-theme*))
      (progn
        (hooks:remove-hook (nyxt:prompt-buffer-make-hook *browser*)
                           'style-prompt)
        (hooks:add-hook (nyxt:prompt-buffer-make-hook *browser*)
                        (make-instance
                         'hooks:handler
                         :fn (lambda (prompt)
                               (setf (nyxt:style prompt)
                                     (compute-style
                                      *current-theme*
                                      :element 'nyxt:prompt-buffer
                                      :accessor #'prompt)))
                         :name 'style-prompt-sans-cut)))
      (progn
        (hooks:remove-hook (nyxt:prompt-buffer-make-hook *browser*)
                           'style-prompt-sans-cut)
        (hooks:add-hook (nyxt:prompt-buffer-make-hook *browser*)
                        (make-instance
                         'hooks:handler
                         :fn (lambda (prompt)
                               (setf (nyxt:style prompt)
                                     (compute-style
                                      *current-theme*
                                      :element 'nyxt:prompt-buffer
                                      :accessor #'prompt)))
                         :name 'style-prompt)))))

(defun reload-hint-style (mode)
  "Reloads hint styles in MODE."
  (when (find-submode (resolve-symbol :web-mode :mode) (buffer mode))
    (setf (nyxt/web-mode:box-style
           (find-submode (resolve-symbol :web-mode :mode) (buffer mode)))
          (compute-style *current-theme*
                         :element 'nyxt/web-mode:web-mode
                         :style-slot 'nyxt/web-mode:box-style
                         :accessor #'hint))))

(defun reload-buffer-style ()
  "Reloads user buffers styles"
  (loop for buffer in (nyxt::buffer-initial-suggestions)
        do (progn
             (setf (nyxt::style buffer) (compute-style *current-theme*
                                                       :element 'nyxt:buffer
                                                       :accessor #'user-buffer))
             (nyxt:buffer-load (nyxt:url buffer) :buffer buffer))))

(define-command-global select-theme (&optional name (mode (current-tailor-mode)))
  "Selects an `internal-theme' with NAME from MODE and applies it."
  (let ((theme (or (and name
                        (find name (themes mode)
                              :key #'name :test #'string=))
                   (nyxt:prompt1
                     :prompt "Select theme"
                     :sources (make-instance 'theme-source)))))
    (setf *current-theme* theme
          (theme *browser*) theme)
    (reload-window-style)
    (reload-prompt-style)
    (reload-hint-style mode)
    (reload-buffer-style)
    theme))

(define-command-global apply-current-theme ()
  "Applies the `current-theme''s color scheme to the current page."
  (when *current-theme*
    (nyxt::html-set-style
     (funcall (user-buffer (cut (current-theme (current-tailor-mode))))
              *current-theme*)
     (current-buffer))))
