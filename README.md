

# nx-tailor

`nx-tailor` is a thin wrapper over the [Nyxt](https://nyxt.atlas.engineer/) built-in theme library (`nyxt/theme`). Its main purpose is to allow handling multiple themes inside a single browser session and to streamline theme selection on startup based on user configuration.  


## Installation

To install the extension, you should download the source and place it in Nyxt's extensions path, given by the value of `nyxt-source-registry` (by default `~/.local/share/nyxt/extensions`).  

    git clone https://github.com/migalmoreno/nx-tailor ~/.local/share/nyxt/extensions/nx-tailor

The extension works with **Nyxt 3 onward** but it's encouraged to use it with the latest version of Nyxt master for the time being. It also requires you to use the [SBCL](https://www.sbcl.org/manual/) Common Lisp implementation to make use of the scheduling capabilities for automatic theme switching.  

If you want to place the extension elsewhere in the system, such as for development purposes, you can configure so via the ASDF source registry mechanism. For this, you'll need to create a file in the source registry directory, `~/.config/common-lisp/source-registry.conf.d/`, and then put the following contents into it, replacing the path with the desired system path.  

    (:tree "/path/to/user/location")

Then, make sure to refresh the ASDF cache via `asdf:clear-source-registry`. ASDF will now be able to find the extension on the custom path. For more information on this utility, please refer to the [ASDF manual](https://asdf.common-lisp.dev/asdf.html).  

By default, Nyxt won't read the custom source registry path we provided, so ensure to include a `reset-asdf-registries` invocation in Nyxt's configuration file too.  

In your Nyxt configuration file, place the following.  

    (define-nyxt-user-system-and-load nyxt-user/tailor
      :depends-on (nx-tailor)
      :components ("tailor.lisp"))

Where `tailor.lisp` is a custom file that you should create relative to Nyxt's configuration directory (`*config-file*`'s pathname by default) to provide the extension settings after the `nx-tailor` system has been successfully loaded. Inside this file, place the following.  

    (define-configuration web-buffer
      ((default-modes `(tailor:tailor-mode ,@%slot-default%))))

In addition, you should provide the extension options, explained in the following section.  

To ensure automatic theme switching works accurately both at startup and through the timers, you should have the following snippet in your configuration **before** the `nyxt-user` system definition shown above.  

    (local-time:reread-timezone-repository)
    (setf local-time:*default-timezone*
          (local-time:find-timezone-by-location-name "<your_timezone>"))


## Configuration

To set up `nx-tailor`, you can write a configuration like this:  

    (define-configuration tailor:tailor-mode
      ((tailor:auto-p :time)
       (tailor:light-theme-threshold (* 8 60 60))
       (tailor:dark-theme-threshold (* 21.5 60 60))
       (tailor:main '(modus-operandi . modus-vivendi))
       (tailor:themes
        (list
         (make-instance 'tailor:user-theme
                        :name 'modus-operandi
                        :background-color "white"
                        :on-background-color "black"
                        :primary-color "#093060"
                        :secondary-color "#dfdfdf"
                        :on-secondary-color "#f0f0f0"
                        :accent-color "#8f0075"
                        :on-accent-color "#005a5f"
                        :font-family "Iosevka")
         (make-instance 'tailor:user-theme
                        :name 'modus-vivendi
                        :dark-p t
                        :background-color "black"
                        :on-background-color "white"
                        :primary-color "#c6eaff"
                        :secondary-color "#323232"
                        :on-secondary-color "#a8a8a8"
                        :accent-color "#afafef"
                        :on-accent-color "#a8a8a8"
                        :font-family "Iosevka")))))

Where `tailor:tailor-mode` slots include:  

-   **`auto-p` (default: `nil`):** if `:time` or `t`, it will set the theme based on the time of the day. If `:gtk`, it will apply it based on the current GTK theme (specifically, it will check the `GTK_THEME` variable).
-   **`light-theme-threshold` (default: `(* 6 60 60)`):** if `auto-p` is set to `:time` or `t`, this indicates the number of seconds after midnight when the light theme should be applied.
-   **`dark-theme-threshold` (default: `(* 21 60 60)`):** if `auto-p` is set to `:time` or `t`, this indicates the number of seconds after midnight when the dark theme should be applied.
-   **`main`:** if `auto-p` is `nil`, this takes a symbol with the theme name to be selected at browser startup. Otherwise, it can take a cons pair of the form `(LIGHT-THEME . DARK-THEME)` for the corresponding light/dark theme names to be picked from the `themes` list when applying `auto-p` logic. If no `main` is supplied, the first non-`:dark-p` theme from `themes` will be chosen as the light theme and the first `:dark-p` theme from `themes` as the dark theme.
-   **`themes`:** a list of `user-theme` themes, each consisting of a mandatory symbol name and the set of theme attributes as per the `theme:theme` constructor.

That's all you need for the extension to work. To see it take effect, simply invoke the `load-theme` command and you'll see the theme change on the fly.  

Now, if you want to tweak Nyxt's appearance further than its color palette by changing some CSS bits, `nx-tailor` provides the `tailor:with-style` macro that takes a class symbol and a list of CSS rules. Essentially, this is a wrapper over `theme:themed-css` which ensures your rules will get applied based on the correct theme. That is, if you include a theme dynamic variable (e.g. `theme:primary`) or a non-atomic expression in your style definition, their value will change depending on the currently-loaded theme. An example style definition in `with-style` form is shown below.  

    (define-configuration status-buffer
      ((style
        (tailor:with-style 'status-buffer
          `("#container"
            :background ,theme:primary
            :color ,(if (theme:dark-p theme:theme)
                        theme:on-background
                        theme:on-accent))
          `("#controls"
            :background "inherit")))))

By doing this, we'll allow `nx-tailor` to handle the styling of the interface element (`nyxt:status-buffer` in the above case).  


## Contributing

Feel free to open an issue with bug reports or feature requests. PRs are more than welcome too.  

