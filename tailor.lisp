(in-package #:nx-tailor)
(nyxt:use-nyxt-package-nicknames)

(defparameter *current-theme* nil
  "Current `user-theme'.")

(defvar *dark-theme-timer* nil
  "`sb-ext:timer' used to apply the dark theme.")

(defvar *light-theme-timer* nil
  "`sb-ext:timer' used to apply the light theme.")

(sera:export-always 'make-theme)
(defun make-theme (name &rest extra-slots &key &allow-other-keys)
  "Builds a `nx-tailor' theme. NAME is required and EXTRA-SLOTS
can vary depending on the theme complexity."
  (apply #'make-instance 'user-theme :name name extra-slots))

(sera:export-always 'make-important)
(defun make-important (value)
  "Outputs a CSS `!important' rule for VALUE."
  (format nil "~a !important" value))

(defun list-of-lists-p (object)
  "Returns non-nil of OBJECT consists of a list of lists."
  (and (listp object)
       (every #'listp object)))

(defun today ()
  "Compute the correct timestamp for today according to the local timezone
 given by `local-time:*default-timezone*'."
  (local-time:adjust-timestamp (local-time:now)
    (set :hour 0)
    (set :minute 0)
    (set :sec 0)
    (set :nsec 0)))

(define-class cut ()
  ((name
    :type (or null list)
    :documentation "The name of the cut.")
   (buffer
    :type (or null list)
    :documentation "A list of CSS rules to tweak the style of `nyxt:buffer'.")
   (prompt
    :type (or null list)
    :documentation "A list of CSS rules to tweak the style of `nyxt:prompt-buffer'.")
   (status
    :type (or null list)
    :documentation "A list of CSS rules to tweak the style of `nyxt:status-buffer'.")
   (message
    :type (or null list)
    :documentation "A list of CSS rules to tweak the style of `nyxt:window'.")
   (hint
    :type (or null list)
    :documentation "A list of CSS rules to tweak the style of `nyxt/hint-mode:hint-mode'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-slot-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A cut is a theme's custom finishing which styles various bits of Nyxt's interface.")
  (:metaclass user-class))

(define-class user-theme (theme:theme)
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

(defun theme-handler (buffer)
  "Handler function to re-calculate styles in BUFFER."
  (setf (nyxt::style buffer) (compute-style
                              *current-theme*
                              :element 'nyxt:buffer
                              :accessor #'buffer))
  (when (find-submode (resolve-symbol :hint-mode :mode) buffer)
    (setf (nyxt/hint-mode:box-style (find-submode (resolve-symbol :hint-mode :mode) buffer))
          (compute-style *current-theme*
                         :element 'nyxt/hint-mode:hint-mode
                         :style-slot 'nyxt/hint-mode:box-style
                         :accessor #'hint))))

(define-mode tailor-mode ()
  "Mode that manages custom browser themes."
  ((themes
    '()
    :type list
    :documentation "`user-theme' objects among which to select the main interface theme.")
   (main
    nil
    :type (or boolean string user-theme cons)
    :documentation "If a single theme, this will be chosen at startup if `auto-p' is `nil'. If a cons
pair and `auto-p' is non-`nil', the light and dark theme variants will be selected from a cons of the form
(LIGHT-THEME . DARK-THEME) where each element is the name of the corresponding theme.")
   (auto-p
    nil
    :type (or boolean :time :gtk)
    :documentation "Whether to automatically apply a `theme'. If `:time' or `t', it will apply itself
based on the time of the day, if `:gtk' it will be applied based on the value
of `GTK_THEME', or if a matching theme name, it will always choose that theme on startup.")
   (light-theme-threshold
    (* 6 60 60)
    :type number
    :documentation "Number of seconds after midnight at which the light theme should be activated.")
   (dark-theme-threshold
    (* 21 60 60)
    :type number
    :documentation "Number of seconds after midnight at which the dark theme should be activated.")))

(defmethod nyxt:customize-instance :after ((mode tailor-mode) &key)
  (with-slots (light-theme-threshold
               dark-theme-threshold
               main
               themes)
      mode
    (flet ((find-theme (name)
             (find name themes :key #'name :test #'string=)))
      (setf light-theme-threshold (round light-theme-threshold))
      (setf dark-theme-threshold (round dark-theme-threshold))
      (when (consp main)
        (setf main (cons (find-theme (car main)) (find-theme (cdr main))))))))

(defmethod nyxt:enable ((mode tailor-mode) &key)
  (with-slots (main auto-p) mode
    (let ((light-theme (or (when (consp main)
                             (car main))
                           (find-theme-variant mode)))
          (dark-theme (or (when (consp main)
                            (cdr main))
                          (find-theme-variant mode :dark t)))
          (light-theme-threshold (local-time:timestamp+ (today) (light-theme-threshold mode) :sec))
          (dark-theme-threshold (local-time:timestamp+ (today) (dark-theme-threshold mode) :sec)))
      (unless (equal auto-p :gtk)
        (unless *light-theme-timer*
          (sb-ext:schedule-timer (setf *light-theme-timer*
                                       (sb-ext:make-timer (lambda ()
                                                            (select-theme (name light-theme) mode))
                                                          :thread t))
                                 (local-time:timestamp-to-universal light-theme-threshold)
                                 :absolute-p t
                                 :repeat-interval (* 24 60 60)))
        (unless *dark-theme-timer*
          (sb-ext:schedule-timer (setf *dark-theme-timer*
                                       (sb-ext:make-timer (lambda ()
                                                            (select-theme (name dark-theme) mode))
                                                          :thread t))
                                 (local-time:timestamp-to-universal dark-theme-threshold)
                                 :absolute-p t
                                 :repeat-interval (* 24 60 60))))
      (unless (or (not (themes mode))
                  (find (theme *browser*) (themes mode) :test #'equal)
                  *current-theme*)
        (or (select-automatic-theme mode)
            (when main
              (typecase (main mode)
                (string
                 (select-theme (find main (themes mode) :key #'name :test #'string=) mode))
                (t
                 (select-theme (name main) mode))))
            (select-theme (name (car (themes mode))) mode))
        (hooks:add-hook (nyxt:buffer-before-make-hook *browser*) #'theme-handler)))))

(defmethod nyxt:disable ((mode tailor-mode) &key)
  (hooks:remove-hook (nyxt:buffer-before-make-hook *browser*) #'theme-handler)
  (hooks:remove-hook (nyxt:prompt-buffer-make-hook *browser*) 'style-prompt-buffer)
  (setf *current-theme* nil)
  (when *light-theme-timer*
    (sb-ext:unschedule-timer *light-theme-timer*))
  (when *dark-theme-timer*
    (sb-ext:unschedule-timer *dark-theme-timer*)))

(defun select-automatic-theme (mode)
  "Automatically set the theme based on the specified criteria in MODE."
  (alex:when-let ((auto (auto-p mode)))
    (let* ((light-theme (or (when (consp (main mode))
                              (car (main mode)))
                            (find-theme-variant mode)))
           (dark-theme (or (when (consp (main mode))
                             (cdr (main mode)))
                           (find-theme-variant mode :dark t)))
           (light-theme-threshold (local-time:timestamp+ (today) (light-theme-threshold mode) :sec))
           (dark-theme-threshold (local-time:timestamp+ (today) (dark-theme-threshold mode) :sec)))
      (case auto
        (:gtk
         (if (or (str:containsp ":light" (uiop:getenv "GTK_THEME"))
                 (null (uiop:getenv "GTK_THEME")))
             (select-theme (name light-theme) mode)
             (select-theme (name dark-theme) mode)))
        (t
         (cond
           ((and (local-time:timestamp> (local-time:now) dark-theme-threshold)
                 (not (local-time:timestamp< (local-time:now) light-theme-threshold)))
            (select-theme (name dark-theme) mode))
           ((local-time:timestamp< (local-time:now) light-theme-threshold)
            (select-theme (name dark-theme) mode))
           ((local-time:timestamp> (local-time:now) light-theme-threshold)
            (select-theme (name light-theme) mode))))))))

(defun find-theme-variant (mode &key dark)
  "Finds the first light theme variant from MODE. If DARK, it finds the first dark theme."
  (if dark
      (find-if #'theme:dark-p (themes mode))
      (find-if-not #'theme:dark-p (themes mode))))

(defun current-tailor-mode ()
  "Returns `tailor-mode' if it's active in the current buffer."
  (find-submode
   (resolve-symbol :tailor-mode :mode '(:nx-tailor))))

(defun compute-style (theme &key element accessor (style-slot nil))
  (str:concat
   (eval (get-original-style element (or style-slot)))
   (and (cut theme) (funcall (eval `(lambda (theme)
                                      (theme:themed-css theme
                                        ,@(funcall accessor (cut theme)))))
                             theme))))

(defmethod reload-style ((element nyxt:window))
  (if (not element)
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

(defmethod reload-style ((element nyxt:prompt-buffer))
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

(defmethod reload-style ((element nyxt/hint-mode:hint-mode))
  (setf (nyxt/hint-mode:box-style element)
        (compute-style *current-theme*
                       :element 'nyxt/hint-mode:hint-mode
                       :style-slot 'nyxt/hint-mode:box-style
                       :accessor #'hint)
        (nyxt/hint-mode:highlighted-box-style element)
        (compute-style *current-theme*
                       :element 'nyxt/hint-mode:hint-mode
                       :style-slot 'nyxt/hint-mode:highlighted-box-style
                       :accessor #'hint)))

(defmethod reload-style ((element nyxt:buffer))
  (loop for buffer in (nyxt::buffer-initial-suggestions)
        do (when (nyxt:internal-url-p (url buffer))
             (setf (nyxt::style buffer) (compute-style *current-theme*
                                                       :element 'nyxt:buffer
                                                       :accessor #'buffer))
             (nyxt:buffer-load (nyxt:url buffer) :buffer buffer))))

(define-command-global select-theme (&optional name (mode (current-tailor-mode)))
  "Selects a `user-theme' with NAME from MODE and applies it."
  (let ((theme (or (and name
                        (find name (themes mode)
                              :key #'name :test #'string=))
                   (nyxt:prompt1
                    :prompt "Select theme"
                    :sources (make-instance 'theme-source))))
        (hint-mode (find-submode (resolve-symbol :hint-mode :mode) (nyxt:buffer mode))))
    (setf *current-theme* theme
          (theme *browser*) theme)
    (reload-style (current-window))
    ;; TODO: don't rely on creating a bogus prompt buffer
    (reload-style (make-instance 'nyxt:prompt-buffer
                                 :window (current-window)
                                 :sources (make-instance 'prompter:raw-source)))
    (when hint-mode
      (reload-style hint-mode))
    (reload-style (current-buffer))
    theme))

(define-command-global apply-current-theme ()
  "Applies the `*current-theme*' color scheme to the current page."
  (when *current-theme*
    (nyxt::html-set-style
     (funcall (buffer (cut *current-theme*))
              *current-theme*)
     (current-buffer))))
