;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(csv
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     chrome
     colors
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     git
     helm
     org
     ;;nlinum
     ranger
     restclient
     search-engine
     (shell :variables
            ;; shell-default-term-shell "xonsh"
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     semantic
     syntax-checking
     ;; version-control

     dap
     lsp
     ;; Languages:
     autohotkey
     c-c++
     emacs-lisp
     html
     java
     javascript
     latex
     markdown
     python
     (rust :variables
           rust-backend 'lsp)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
     pretty-mode
     (prettify-utils :location (recipe :fetcher github
                                       :repo "Ilazki/prettify-utils.el")))
   ;; A list of packages that cannot be updated.
   ;; I manually modified solarized.el to high-contrast as according to:
   ;; https://github.com/altercation/vim-colors-solarized/blob/master/colors/solarized.vim#L399-L405
   dotspacemacs-frozen-packages '(solarized-theme)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(exec-path-from-shell)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(load (expand-file-name "init_vars.el" dotspacemacs-directory))

(defun dotspacemacs/user-env ()
   "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
   (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (add-hook 'hack-local-variables-hook (lambda () (spacemacs/toggle-fill-column-indicator-on)))
  (add-hook 'hack-local-variables-hook (lambda () (spacemacs/toggle-visual-line-navigation-on)))
  (spacemacs/declare-prefix "o" "user-defined-prefix")
  (spacemacs/set-leader-keys "og" 'engine/search-google)
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  (spacemacs/toggle-indent-guide-globally)

  (load (expand-file-name "prettify.el" dotspacemacs-directory))
  (module/display)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; Native line numbers
  (add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
  (add-hook 'text-mode-hook (lambda () (setq display-line-numbers 'relative)))
  )
