(defun my-evil-config ()
  "Evil-specific/internal setup"
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (global-set-key (kbd "C-;") 'eval-expression)



  ;; This was a mess. See:
  ;; https://github.com/syl20bnr/spacemacs/pull/10155
  ;; https://github.com/emacs-evil/evil/pull/1059

  ;; setup evil-minibuffer
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html
  ;; WARNING: With lexical binding, lambdas from `mapc' and `dolist' become
  ;; closures in which we must use `evil-define-key*' instead of
  ;; `evil-define-key'.
  (dolist (map (list minibuffer-local-map
                     minibuffer-local-ns-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map
                     minibuffer-local-isearch-map))
    (evil-define-key* 'normal map (kbd "<escape>") 'abort-recursive-edit)
    (evil-define-key* 'normal map (kbd "<return>") 'exit-minibuffer)
    ;; C-r and C-s to search forward/backward are important in minibuffer
    (evil-define-key* 'insert map (kbd "C-r") 'isearch-backward)
    (evil-define-key* 'insert map (kbd "C-s") 'isearch-forward))

  (add-hook
   'minibuffer-setup-hook
   (lambda ()
     (set (make-local-variable 'evil-echo-state) nil)
     ;; (evil-set-initial-state 'mode 'insert) is the evil-proper
     ;; way to do this, but the minibuffer doesn't have a mode.
     ;; The alternative is to create a minibuffer mode (here), but
     ;; then it may conflict with other packages' if they do the same.
     (evil-insert 1)
     (evil-normalize-keymaps)))

  (setq evil-want-minibuffer t)

  ;; Because of the above minibuffer-setup-hook, some evil-ex bindings need be reset.
  (evil-define-key 'normal evil-ex-completion-map (kbd "<escape>") 'abort-recursive-edit)
  (evil-define-key 'insert evil-ex-completion-map (kbd "C-p") 'previous-complete-history-element)
  (evil-define-key 'insert evil-ex-completion-map (kbd "C-n") 'next-complete-history-element)
  (evil-define-key 'normal evil-ex-completion-map (kbd "C-p") 'previous-history-element)
  (evil-define-key 'normal evil-ex-completion-map (kbd "C-n") 'next-history-element)

  ;; Rebind C-k in minibuffer map; conflict with evil minibuffer
  (with-eval-after-load 'helm
    (evil-define-key 'insert helm-map (kbd "C-k") 'helm-previous-line)
    (evil-define-key 'normal helm-map (kbd "C-z") 'helm-select-action)
    (evil-define-key 'insert helm-map (kbd "C-z") 'helm-select-action)
    (evil-define-key 'insert helm-map (kbd "C-p") 'helm-previous-line)
    (evil-define-key 'normal helm-map (kbd "C-p") 'helm-previous-line)
    (evil-define-key 'insert helm-map (kbd "C-n") 'helm-next-line)
    (evil-define-key 'normal helm-map (kbd "C-n") 'helm-next-line)
    (evil-normalize-keymaps))

  )
