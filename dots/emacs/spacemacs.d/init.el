;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Using PLISTS is recommented for performance reasons, BUT
;; code lens do not work when using plists ... (wat?!)
;; (setenv "LSP_USE_PLISTS" "true")

(setq kzk/hack-original-display (getenv "DISPLAY"))
(defun kzk/hack-reset-original-display-env-var (&optional frame)
  ;;; the message will insist it's resetting from :0 to :0, but if this is not
  ;;; executed, (getenv "DISPLAY") will return the wrong value because reasons
  (message "Resetting DISPLAY from %S to %S" (getenv "DISPLAY") kzk/hack-original-display)
  (run-at-time "3 seconds" nil (lambda () (setenv "DISPLAY" kzk/hack-original-display)))
  (remove-hook 'after-make-frame-functions #'kzk/hack-reset-original-display-env-var)
  "$DISPLAY kludge"
  )

(add-hook 'after-make-frame-functions #'kzk/hack-reset-original-display-env-var)

;; (message "early display is %S in process %S" (getenv "DISPLAY") (emacs-pid))
;; keep around for debug, may be necessary
;; (defun handle-set-env (VARIABLE &optional VALUE SUBSTITUTE-ENV-VARS)

;;   (when (equal VARIABLE "DISPLAY")
;;     (require 'backtrace)
;;     (message "Calling setenv %S with value %S subs %S in process %S" VARIABLE VALUE SUBSTITUTE-ENV-VARS (emacs-pid))
;;     (message "from %S" (backtrace-to-string)))
;;   )
;; (defun handle-set-env-internal (ENV VARIABLE &optional VALUE KEEP-EMPTY)

;;   (when (equal VARIABLE "DISPLAY")
;;     (message "Calling setenv-internal %S with value %S keep empty %S in process %S" VARIABLE VALUE KEEP-EMPTY (emacs-pid))
;;     (require 'backtrace)
;;     (message "from %S" (backtrace-to-string)))
;;   )

;; (advice-add 'setenv :before 'handle-set-env)
;; (advice-add 'setenv-internal :before 'handle-set-env-internal)

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native compilation is available")
      (setq
       comp-deferred-compilation t
       ;; Avoid compilation taking all computing resources in the computer
       native-comp-async-jobs-number 2))
  (message "Native complation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(add-to-list 'load-path (expand-file-name "~/.spacemacs.d/lisp"))
(require 'kzk-utils)

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(csv
     html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (unicode-fonts :variables unicode-fonts-enable-ligatures t)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     nav-flash
     theming
     compleseus
     ;; syntax-checking
     (auto-completion :variables
                      auto-completion-front-end 'company
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip nil
                      auto-completion-use-company-posframe t
                      ;; auto-completion-use-company-box t
                      ;; auto-completion-enable-sort-by-usage t
                      )
     multiple-cursors
     (git :variables
          git-enable-magit-gitflow-plugin t
          magit-bury-buffer-function 'magit-mode-quit-window)
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin nil
                      version-control-diff-side 'left)
     ;; (treemacs :variables treemacs-use-git-mode 'deferred)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     ;; enable-flyspell-auto-completion t
                     )
     dtrt-indent
     (lsp :variables
          lsp-keymap-prefix "C-c C-l"
          lsp-keep-workspace-alive nil
          ;; lsp-ui-doc-enable nil
          lsp-idle-delay 1
          lsp-ui-doc-delay 1
          lsp-ui-doc-position 'at-point
          lsp-use-upstream-bindings nil
          ;; hint from https://github.com/emacs-lsp/lsp-pyright/issues/66
          ;; trying multiple instances to see if improves performance
          ;; when not all of the projects are opened
          lsp-pyright-multi-root nil
          lsp-enable-snippet nil ;; Disable snippets in LSP, as it becomes increasingly annoying!
          )

     ;; testing tree-sitter
     (tree-sitter :variables
                  spacemacs-tree-sitter-hl-black-list '(markdown-mode)
                  tree-sitter-syntax-highlight-enable t
                  tree-sitter-fold-enable t
                  tree-sitter-fold-indicators-enable nil
                  )
     kzk-tree-sitter

     emacs-lisp
     shell-scripts
     (ipython-notebook)
     (python :variables
             ;; lsp-pyright-log-level "trace"
             lsp-pyright-venv-path "/home/kassick/.pyenv/versions"
             python-auto-set-local-pyenv-version nil
             python-auto-set-local-pyvenv-virtualenv nil
             python-test-runner 'pytest
             python-backend 'lsp
             python-lsp-server 'pyright
             python-spacemacs-indent-guess nil
             python-formatter 'black
             python-indent-def-block-scale 1)
     rust
     (go :variables
         go-backend 'lsp
         ;; go-format-before-save t
         go-tab-width 4
         ;; go-use-golangci-lint t
         company-go-insert-arguments nil
         lsp-go-use-placeholders nil
         )
     kzk-go
     (c-c++ :variables c-c++-backend 'lsp-clangd)
     cmake
     ;; (scala :variables
     ;;        scala-backend 'scala-metals
     ;;        lsp-metals-server-command "~/.local/dev/metals/metals"
     ;;        scala-auto-start-backend t
     ;;        scala-enable-eldoc t)
     ;; (rust :variables rust-backend 'lsp)

     ;; JS thingies
     prettier
     (json :variables
           json-backend 'lsp
           json-fmt-on-save nil
           json-fmt-tool 'prettier
           )
     javascript
     (typescript ;; :variables
                 ;; typescript-linter 'eslint ; let lsp to the linting
                 )
     tide
     (groovy :variables
             groovy-lsp-jar-path "/home/kassick/Sources/user/groovy-language-server/build/libs/groovy-language-server-all.jar")
     terraform
     ;; cscope
     ;; fsharp
     ;; csharp
     ;; java
     ;; antlr
     ;; (haskell :variables haskell-completion-backend 'intero)
     ;; javascript
     ;; graphviz
     ;; pandoc
     ;; pdf-tools
     yaml
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :variables
          ;; org-hide-emphasis-markers t
          ;; org-enable-appear-support t
          ;; org-appear-trigger 'always ;;manual
          org-enable-modern-support t
          org-enable-valign t)
     (latex :variables
            latex-enable-folding t
            latex-enable-auto-fill nil)
     (bibtex)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)

     kzk-general
     kzk-evil
     kzk-window-management
     kzk-projectile
     kzk-code-tweaks
     kzk-mc
     kzk-embrace
     kzk-magit
     kzk-org
     kzk-lsp
     kzk-makefile
     kzk-python
     kzk-ahs
     kzk-visual-editing ; fix adaptive wrap for org
     kzk-compleseus
     kzk-company
     kzk-persp
     spacemacs-editing
     spacemacs-visual
     spacemacs-editing-visual
     ;; tabs
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      ;; memory-usage
                                      ;; gcmh
                                      diminish
                                      key-chord
                                      yasnippet-snippets
                                      edit-indirect ;; required by markdown
                                      envrc
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    ;; ido and ido-vertical and what-more bind some keys that should belong to helm
                                    ido
                                    ido-vertical
                                    flx-ido
                                    ;; I prefer org in text mode
                                    org-bullets
                                    ;;; helm-ls-git is kidnapping git-rebase buffers
                                    ;;; and screwing with the experience
                                    ;;; https://github.com/syl20bnr/spacemacs/issues/15089
                                    helm-ls-git
                                    term-cursor  ;; the termcursor hook is
                                                 ;; misbehaving. I mostly use
                                                 ;; the GUI and I can live
                                                 ;; without it
                                    mmm-mode     ;; mmm-mode causes strange behaviour with markdown. It's no longer necessary, apparently
                                    importmagic  ;; causes some lag; not used
                                    flyspell-correct-popup
                                    evil-escape  ;; STOP THIS NONSENSE
                                    )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   ;; dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-install-packages 'used-only
   ))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default          '(100000000 0.1))
   ;; dotspacemacs-gc-cons '(1000000000 0.1)
   ;; gc-cons-threshold  10000000000
   ;; gc-cons-percentage 0.6
   ;; garbage-collection-messages t


   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'lisp-interaction-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark spacemacs-light modus-vivendi ample modus-operandi )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   dotspacemacs-mode-line-theme '(doom)
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator-scale 1.2)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '(("Iosevka" :weight light :width expanded :size 9.8)
                               ;; ("Fira Code" :height 120)
                               ;; ("Meslo LG S for Powerline"
                               ;;  :height 113
                               ;;  :powerline-scale 1.0)
                               ;; ("Monego"
                               ;;  :height 120
                               ;;  :weight normal
                               ;;  :width normal
                               ;;  :powerline-scale 1.0)
                               ;; ("Monaco"
                               ;;  :height 120
                               ;;  :weight normal
                               ;;  :width normal
                               ;;  :powerline-scale 1.0)
                               ("Dejavu Sans Mono"
                                :size 9.8
                                :weight regular
                                :width normal
                                :powerline-scale 1.0)
                               )

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   ;; dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "M-M"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   vim-style-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   vim-style-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   vim-style-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   vim-style-ex-substitute-global nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil

   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil

   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)

   dotspacemacs-helm-position 'bottom

   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'source

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   ;; WARNING setting it to t causes paste to stop working
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative nil
                               :visual nil
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   pdf-view-mode)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I - %b (%t)"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode nil

   ;; Accept SPC as y for prompts if non nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil
  )

  ;; General settings
  ;; (custom-set-variables
  ;;  `(undo-tree-history-directory-alist '(("." . ,(expand-file-name "~/tmp")))))

  ;; end
  )

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (load-env-vars spacemacs-env-vars-file) ;; force loading of envs
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (custom-set-variables
   ;; trying to see if ediff is the one causing me headaches with surrogate minibuffer frames
   '(ediff-window-setup-function (quote ediff-setup-windows-plain))

   ;; emacs 29 prefers completing-read for answers, which does not feel so good with helm
   '(use-short-answers t)

   ;; not using this causes vterm to use "wrong" colors -- most notably, the
   ;; current path in frontcube's zsh theme. See
   ;; https://github.com/akermu/emacs-libvterm/issues/549
   '(vterm-set-bold-hightbright t)
   )

  (setq theming-modifications
        `(

          (modus-operandi
           (default :foreground "#000000")
           )
          ;; --- Ample theme ---
          (ample
           ;; fix some way-too-dark settings on ample
           (hl-line :background "gray20")
           (header-line :background "#3d3d3f"
                        :foreground "white smoke"
                        :weight semi-bold)
           (region :background "gray35")
           (helm-ff-dotted-directory :foreground "#6aaf50" :weight bold)
           (helm-ff-dotted-symlink-directory :foreground "DarkOrange" :weight bold)
           (evil-ex-lazy-highlight :foreground "white" :background "DarkOliveGreen4")
           (highlight :background "gray30")
           (aw-leading-char-face :height 2.0 :foreground "red")
           )

          (ample-light
           (default :foreground "#0A0A0A")
           )

          ;; --- Spacemacs Light ---
          (spacemacs-light

           ;; fix latex bold looking weird in light theme
           (font-latex-bold-face :foreground "#6c4173"
                                 :weight bold)

           ;; fix some issues with auto-highlight-symbol default faces vs spacemacs-light
           ;; fix auto-highlight looking exagerated and hiding the cursor under the char
           (ahs-plugin-default-face :foreground "black"
                                    :background "papaya whip")
           (ahs-plugin-default-face-unfocused :foreground "black"
                                              :background "antique white")
           (ahs-face :inherit ahs-plugin-default-face)
           (ahs-face-unfocused :inherit ahs-plugin-default-face-unfocused)

           ;; Fix using the same color as visual selection, as then we can not
           ;; see what is being selected when lsp-mode highlights the current
           ;; symbol
           (highlight :background "papaya whip"
                      :foreground "#655370"))

          ;; --- Spacemacs Dark ---
          (spacemacs-dark

           ;; Fix using the same color as visual selection, as then we can not
           ;; see what is being selected when lsp-mode highlights the current
           ;; symbol
           (highlight :background "#5f5c70"
                      :foreground "#b2b2b2")
           (default :foreground "#c1c1c1")
           (scrollbar :foreground "gray52")
           (vterm-color-black :foreground "#304040" :background "#708284")
           (vterm-color-blue  :foreground "#42A5F5" :background "#42A5F5")
           (vterm-color-cyan  :foreground "#00ACC1" :background "#00ACC1")
           (vterm-color-green :foreground "#C3D82C" :background "#C3D82C")
           (vterm-color-white :foreground "#DEDDDA" :background "#DEDDDA")
           (completions-annotations :foreground "#a3a3a3" :inherit (italic shadow))
           )

          ;; --- END theming-modifications ---
          ))


  (require 'kzk-gui-tweaks)
  (require 'kzk-smart-home-end)
  (kzk-smart-home-end-global-mode)
  (require 'kzk-smart-c-backspace)

  ;; (require 'kzk-latex)   ; latex-mode settings
  ;; (require 'kzk-headers) ; headers for files

  ;; Extra code and customizations
  ;;(custom-set-faces '(default ((t (:slant normal :weight normal :height 130 :width normal :foundry "ADBO" :family "Meslo LG S for Powerline")))))
  ;                  '(region ((t (:background "gray31"))))
  ;                  '(helm-ff-dotted-directory ((t (:foreground "#6aaf50" :weight bold))))
  ;                  '(helm-ff-dotted-symlink-directory ((t (:foreground "DarkOrange" :weight bold)))))

  ;; Leave customizations out of the git tree. Make sure the file exists
  (let ((cf  (expand-file-name "~/.spacemacs.d/customize.el") ))
    (unless (file-exists-p cf)
      (write-region "" nil cf))
    (setq custom-file cf))
  (load-file custom-file)

  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (message "Running user config")


  ;; Default modes
  (ido-mode -1)          ; no ido
  (key-chord-mode 1)     ; keychord integrates with general
  (show-paren-mode t)
  (global-page-break-lines-mode 1)
  (envrc-global-mode)

  (add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

  ;; this one has to be kept here, as if it's loaded on layers, somehow it does
  ;; not respect inclusivity
  (require 'kzk-evil-easymotion)
  ;;(require 'kzk-shell)   ; shell tweaks

  ;; Language tweaks
  (setq default-process-coding-system '(utf-8 . utf-8)) ; utf8
  (set-language-environment "UTF-8")                    ; utf8
  (require 'iso-transl)                                 ; see https://www.emacswiki.org/emacs/DeadKeys

  ;; Stop littering everywhere with backups and locks!
  (setq backup-directory-alist '(("." . "~/tmp")))

  ; Visual settings
  (setq-default line-spacing 0        ; Increase line-spacing (default 0)
                fill-column 78        ; reasonable fill column
                indent-tabs-mode nil  ; tabs off by default
                tab-width 4           ; reasonable tab-width
                powerline-default-separator 'arrow ; Don't use separators
                )

  ;; ;; Give us narrowing back!
  ;; (put 'narrow-to-region 'disabled nil)
  ;; (put 'narrow-to-page 'disabled nil)
  ;; (put 'narrow-to-defun 'disabled nil)
  ;; (put 'narrow-to-region 'disabled nil)
  ;; (put 'scroll-left 'disabled nil)
  ;; (with-eval-after-load 'latex
  ;;   (put 'LaTeX-narrow-to-environment 'disabled nil)
  ;;   )

  ;; Give us region-case back
  ;; (put 'upcase-region 'disabled nil)
  ;; (put 'downcase-region 'disabled nil)

  ;; (with-eval-after-load 'helm (helm-mode t))

  ;; Don't be annoying, emacs. C-x C-c is to close to EVERY OTHER SHORTCUT and
  ;; can be used accidently too often. Also, no C-g on the confirmation prompt!?
  ;; what the hell?
  (global-unset-key (kbd "C-x C-c"))

  ;; No surprise iconify
  (global-unset-key (kbd "C-x C-z"))

  ;; I always press C-x s by accident. I NEVER intend to call
  ;; save-some-buffers
  (global-set-key (kbd "C-x s") 'save-buffer)
  (global-set-key (kbd "C-x S") 'save-some-buffers)

  ;; (message "setting gcmh mode")
  ;; (gcmh-mode t)

  ;; dotspacemacs/user-config ends here
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil))
)
