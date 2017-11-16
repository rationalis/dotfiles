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
   '(
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
     ;; nlinum
     ranger
     restclient
     search-engine
     (shell :variables
            ;; shell-default-term-shell "xonsh"
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; version-control

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
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
     pretty-mode
     (prettify-utils :location (recipe :fetcher github
                                       :repo "Ilazki/prettify-utils.el"))
     (xelb :location (recipe :fetcher github
                             :repo "ch11ng/xelb"))
     (exwm :location (recipe :fetcher github
                             :repo "ch11ng/exwm"))
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(load (expand-file-name "init_vars.el" dotspacemacs-directory))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
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

  ;; Native line numbers
  (add-hook 'hack-local-variables-hook (lambda () (setq display-line-numbers 'relative)))
  (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))
  (add-hook 'text-mode-hook (lambda () (display-line-numbers-mode)))

  (set-face-attribute 'line-number-current-line nil
                      :background "white" :foreground "black")

  (spacemacs/declare-prefix "o" "user-defined-prefix")
  (spacemacs/set-leader-keys "og" 'engine/search-google)
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  (spacemacs/toggle-indent-guide-globally)

  (load (expand-file-name "prettify.el" dotspacemacs-directory))
  (module/display)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)


  ;; Workaround solution to Helm taking up full width and hiding all other windows
  ;; See https://github.com/syl20bnr/spacemacs/issues/9044
  (setq helm-split-window-inside-p t)
  (setq-default helm-display-function 'helm-default-display-buffer)

  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  (spacemacs/set-leader-keys "oo" (lambda (command)
                                    (interactive (list (read-shell-command "$ ")))
                                    (start-process-shell-command command nil command)))

  (spacemacs/set-leader-keys "of" (lambda () (interactive)
                                    (start-process-shell-command "firefox-developer" nil
                                                                 "firefox-developer")))
  (require 'exwm)
  (require 'exwm-config)
  (setq exwm-input--line-mode-passthrough t)

  ;; Set the initial workspace number.
  ;; (setq exwm-workspace-number 2)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (setq exwm-input--line-mode-passthrough t)
              (exwm-workspace-rename-buffer exwm-class-name)
              ))


  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (exwm-input-set-key (kbd "s-R") #'exwm-restart)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "C-'") #'spacemacs/default-pop-shell)

  ;; 's-w': Switch workspace
  ;; (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-w") #'spacemacs/workspaces-transient-state/body)
  ;; s-h, s-j, s-k, s-l: move around
  (exwm-input-set-key (kbd "s-j") #'evil-window-left)
  (exwm-input-set-key (kbd "s-k") #'evil-window-down)
  ;; doesn't work in VM due to Windows capturing s-l
  (exwm-input-set-key (kbd "s-l") #'evil-window-up) 
  (exwm-input-set-key (kbd "s-;") #'evil-window-right)
  ;; lock screen
  (exwm-input-set-key (kbd "C-M-l") #'lock-screen)

  ;; (exwm-input-set-key (kbd "C-SPC") )

  (define-key global-map (kbd "C-M-l") #'lock-screen)
  (push ?\s-\  exwm-input-prefix-keys)

  ;; Common binding for "History" in web browsers.
  (delete ?\C-h exwm-input-prefix-keys)

  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                     (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)
  ;; Enable EXWM
  (exwm-enable)
  ;; Configure Ido
  (exwm-config-ido)
  ;; Other configurations
  (exwm-config-misc)
  )
