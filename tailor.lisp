(in-package #:nx-tailor)
(nyxt:use-nyxt-package-nicknames)

(defvar *styles* '()
  "The list of `style' objects the user instantiates
by including `with-style' invocations in their configuration.")

(defvar *current-theme* nil
  "The currently-active `user-theme'.")

(defvar *dark-theme-timer* nil
  "`sb-ext:timer' used to apply the dark theme.")

(defvar *light-theme-timer* nil
  "`sb-ext:timer' used to apply the light theme.")

(sera:export-always 'make-theme)
(defun make-theme (id &rest extra-slots &key &allow-other-keys)
  "Build a `nx-tailor' theme. ID is required and EXTRA-SLOTS
will be supplied to the `theme:theme' constructor"
  (apply #'make-instance 'user-theme :id id extra-slots))

(defun today ()
  "Compute the correct timestamp for today according to
 the local timezone given by `local-time:*default-timezone*'."
  (local-time:adjust-timestamp (local-time:now)
    (set :hour 0)
    (set :minute 0)
    (set :sec 0)
    (set :nsec 0)))

(define-class theme-source (prompter:source)
  ((prompter:name "User themes")
   (prompter:constructor (themes (current-tailor-mode)))
   (prompter:active-attributes-keys '("Id"))))

(define-class style ()
  ((sym
    nil
    :type (or null symbol)
    :documentation "The symbol of the class the style belongs to.")
   (fn
    nil
    :type (or null function)
     :documentation "A function that takes a theme and returns a
new style based on its value."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class user-theme (theme:theme)
  ((id
    nil
    :type (or null symbol)
    :documentation "The theme identifier to use for setting
criteria in `tailor-mode'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun current-tailor-mode ()
  "Return `tailor-mode' if it's active in the current buffer."
  (alex:when-let ((mode (sym:resolve-symbol :tailor-mode :mode '(:nx-tailor))))
    (find-submode mode)))

(sera:export-always 'with-style)
(defmacro with-style (class-sym &body rules)
  "Add style RULES to CLASS-SYM and apply RULES.
This automatically adds the user-defined style to the list of
`*styles*' with the CLASS-SYM identifier and a function that takes
a single argument for the current theme to apply style RULES to."
  `(progn
     (pushnew
      (make-instance 'style
                     :sym ,class-sym
                     :fn (lambda (theme)
                           (theme:themed-css theme
                             ,@rules)))
      *styles* :key #'sym)
     (str:concat nyxt:%slot-default%
                 (theme:themed-css (theme *browser*)
                   ,@rules))))

(defun compute-current-style (style obj &optional style-slot)
  "Return the current STYLE of OBJ.
Optionally, retrieve the original style of OBJ via STYLE-SLOT."
  (str:concat (eval (getf (mopu:slot-properties
                           (find-class (class-name (class-of obj)))
                           (or style-slot 'nyxt:style))
                          :initform))
              (when style
                (funcall (fn style) *current-theme*))))

(defgeneric load-style (style interface)
  (:documentation "Load the STYLE of INTERFACE."))

(defmethod load-style (style (buffer nyxt:web-buffer))
  (flet ((style-web-buffer (buffer)
           (setf (nyxt:style buffer)
                 (compute-current-style style buffer))))
    (when (nyxt:internal-url-p (url buffer))
      (style-web-buffer buffer))
    (hooks:add-hook
     (nyxt:buffer-before-make-hook *browser*)
     (make-instance 'hooks:handler
                    :fn #'style-web-buffer
                    :name 'style-web-buffer))
    (loop for buffer in (nyxt::buffer-initial-suggestions)
          do (when (nyxt:internal-url-p (url buffer))
               (style-web-buffer buffer)
               (nyxt:buffer-load (url buffer) :buffer buffer)))))

(defmethod load-style (style (prompt-buffer nyxt:prompt-buffer))
  (flet ((style-prompt-buffer (prompt)
           (setf (nyxt:style prompt)
                 (compute-current-style style prompt-buffer))))
    (style-prompt-buffer prompt-buffer)
    (hooks:add-hook
     (nyxt:prompt-buffer-make-hook *browser*)
     (make-instance 'hooks:handler
                    :fn #'style-prompt-buffer
                    :name 'style-prompt-buffer))))

(defmethod load-style (style (window nyxt:window))
  (flet ((style-message-buffer (window)
           (setf (nyxt::message-buffer-style window)
                 (compute-current-style
                  style window
                  'nyxt:message-buffer-style))))
    (style-message-buffer window)
    (hooks:add-hook
     (nyxt:window-make-hook *browser*)
     (make-instance 'hooks:handler
                    :fn #'style-message-buffer
                    :name 'style-message-buffer))
    (nyxt:echo "")))

(defmethod load-style (style (status-buffer nyxt:status-buffer))
  (flet ((style-status-buffer (status-buffer)
           (setf (nyxt:style status-buffer)
                 (compute-current-style style status-buffer))))
    (style-status-buffer status-buffer)
    (hooks:add-hook
     (nyxt:window-make-hook *browser*)
     (make-instance 'hooks:handler
                    :fn #'style-status-buffer
                    :name 'style-status-buffer))
    (nyxt::print-status)))

(defmethod load-style (style (nyxt-mode nyxt:mode))
  (flet ((style-mode (mode)
           (setf (slot-value mode 'nyxt:style)
                 (compute-current-style style mode))))
    (hooks:once-on (nyxt:buffer-loaded-hook (buffer nyxt-mode)) (_)
      (style-mode nyxt-mode))
    (hooks:add-hook (nyxt:enable-mode-hook (buffer nyxt-mode))
                    (make-instance 'hooks:handler
                                   :fn #'style-mode
                                   :name 'style-mode))))

(define-mode tailor-mode ()
  "Manage and apply user-defined browser themes on predefined criteria."
  ((themes
    '()
    :type list
    :documentation "`user-theme' objects among which to select the
 main interface theme.")
   (main
    nil
    :type (or symbol :dark :light cons null)
    :documentation "If a single theme id is specified, it will be chosen
at startup if `auto-p' is `nil'. If either `:dark' or `:light' is passed,
the corresponding `user-theme' will be selected at startup.
If a cons pair is specified and `auto-p' is non-`nil', the light and dark
theme variants will be selected from the pair in the form
(LIGHT-THEME . DARK-THEME) where each cell is the id of the theme.")
   (auto-p
    nil
    :type (or boolean :time :gtk)
    :documentation "Whether to automatically apply a `theme'. If `:time' or `t',
it will apply the corresponding theme automatically based on the time of the day.
If `:gtk' it will be applied based on the value of the `GTK_THEME' environment
variable.")
   (light-theme-threshold
    (* 6 60 60)
    :type number
    :documentation "Number of seconds after midnight when the light theme
should be activated.")
   (dark-theme-threshold
    (* 21 60 60)
    :type number
    :documentation "Number of seconds after midnight when the dark theme
should be activated.")
   (nyxt:glyph "⏾")))

(defmethod nyxt:customize-instance :after ((mode tailor-mode) &key)
  (with-slots (light-theme-threshold
               dark-theme-threshold
               main
               themes)
      mode
    (flet ((find-theme (id)
             (find id themes :key #'id)))
      (setf light-theme-threshold (round light-theme-threshold))
      (setf dark-theme-threshold (round dark-theme-threshold))
      (when (consp main)
        (setf main (cons (find-theme (car main)) (find-theme (cdr main))))))))

(defun find-theme-variant (mode &key dark)
  "Find the first light `user-theme' in MODE.
If DARK, find the first dark `user-theme'."
  (if dark
      (find-if #'theme:dark-p (themes mode))
      (find-if-not #'theme:dark-p (themes mode))))

(defmethod nyxt:enable ((mode tailor-mode) &key)
  (with-slots (main themes auto-p) mode
    (let ((light-theme (or (when (consp main)
                             (car main))
                           (find-theme-variant mode)))
          (dark-theme (or (when (consp main)
                            (cdr main))
                          (find-theme-variant mode :dark t)))
          (light-theme-threshold
            (local-time:timestamp+ (today) (light-theme-threshold mode) :sec))
          (dark-theme-threshold
            (local-time:timestamp+ (today) (dark-theme-threshold mode) :sec)))
      (unless (or (not themes)
                  (find (theme *browser*) themes :test #'equal)
                  *current-theme*)
        (or (load-automatic-theme mode)
            (when main
              (load-theme
               (id
                (case main
                  (:light light-theme)
                  (:dark dark-theme)
                  (t (find main themes :key #'id))))
               mode))
            (load-theme (id (car themes)) mode)))
      (unless (or (not auto-p) (equal auto-p :gtk))
        (flet ((set-timer (timer theme threshold)
                 (unless timer
                   (sb-ext:schedule-timer
                    (setf timer (sb-ext:make-timer
                                 (lambda ()
                                   (load-theme (id theme) mode))
                                 :thread t))
                    (local-time:timestamp-to-universal threshold)
                    :absolute-p t
                    :repeat-interval 86400))))
          (hooks:once-on (nyxt:buffer-loaded-hook (buffer mode)) (_)
            (set-timer *light-theme-timer* light-theme light-theme-threshold)
            (set-timer *dark-theme-timer* dark-theme dark-theme-threshold)))))))

(defmethod nyxt:disable ((mode tailor-mode) &key)
  (hooks:remove-hook (nyxt:buffer-before-make-hook *browser*) 'style-web-buffer)
  (hooks:remove-hook (nyxt:prompt-buffer-make-hook *browser*) 'style-prompt-buffer)
  (hooks:remove-hook (nyxt:window-make-hook *browser*) 'style-message-buffer)
  (hooks:remove-hook (nyxt:window-make-hook *browser*) 'style-status-buffer)
  (setf (theme *browser*) *current-theme*)
  (setf *current-theme* nil)
  (when *light-theme-timer*
    (sb-ext:unschedule-timer *light-theme-timer*))
  (when *dark-theme-timer*
    (sb-ext:unschedule-timer *dark-theme-timer*)))

(define-command-global load-theme (&optional id (mode (current-tailor-mode)))
  "Load a custom `user-theme' with ID from MODE, apply it, and return it."
  (flet ((find-style (sym)
           (find sym *styles* :key #'sym)))
    (let ((theme (or (and id (find id (themes mode) :key #'id))
                     (nyxt:prompt1
                      :prompt "Load theme"
                      :sources (make-instance 'theme-source))))
          (prompt-buffer (make-instance
                          'nyxt:prompt-buffer
                          :window (current-window)
                          :sources (make-instance 'prompter:raw-source)))
          (modes-with-style
            (remove-if-not (lambda (mode)
                             (some (lambda (slot)
                                     (eq 'nyxt:style slot))
                                   (mopu:slot-names (class-of mode))))
                           (nyxt:modes (nyxt:buffer mode)))))

      (setf *current-theme* theme)
      (setf (theme *browser*) theme)
      (load-style (find-style 'nyxt:window) (current-window))
      (load-style (find-style 'nyxt:web-buffer) (nyxt:buffer mode))
      (load-style (find-style 'nyxt:status-buffer) (nyxt:status-buffer (current-window)))
      (load-style (find-style 'nyxt:prompt-buffer) prompt-buffer)
      (loop for mode-style in modes-with-style
            do (load-style (find-style (class-name (class-of mode-style))) mode-style))
      theme)))

(defmethod load-automatic-theme ((mode tailor-mode))
  "Automatically set the theme based on the specified criteria in MODE."
  (alex:when-let ((auto (auto-p mode)))
    (let* ((light-theme (or (when (consp (main mode))
                              (car (main mode)))
                            (find-theme-variant mode)))
           (dark-theme (or (when (consp (main mode))
                             (cdr (main mode)))
                           (find-theme-variant mode :dark t)))
           (light-theme-threshold (local-time:timestamp+
                                   (today) (light-theme-threshold mode) :sec))
           (dark-theme-threshold (local-time:timestamp+
                                  (today) (dark-theme-threshold mode) :sec)))
      (case auto
        (:gtk
         (if (or (str:containsp ":light" (uiop:getenv "GTK_THEME"))
                 (null (uiop:getenv "GTK_THEME")))
             (load-theme (id light-theme) mode)
             (load-theme (id dark-theme) mode)))
        (t
         (cond
           ((and (local-time:timestamp> (local-time:now) dark-theme-threshold)
                 (not (local-time:timestamp< (local-time:now) light-theme-threshold)))
            (load-theme (id dark-theme) mode))
           ((local-time:timestamp< (local-time:now) light-theme-threshold)
            (load-theme (id dark-theme) mode))
           ((local-time:timestamp> (local-time:now) light-theme-threshold)
            (load-theme (id light-theme) mode))))))))
