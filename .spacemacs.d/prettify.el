;; A working subset for prettification of the .spacemacs config found at:
;; https://github.com/ekaschalk/dotspacemacs

(defun module/display ()
  (module/display/fontsets)
  (module/display/font-locks)
  (module/display/prettify-symbols)
  )

;;;; Fontsets

(defun module/display/fontsets ()
  "Set right fonts for missing and all-the-icons unicode points."

  ;; Fira code ligatures. Fira Code Symbol is a different font than Fira Code!
  ;; You can use any font you wish with just the ligatures, I use Hack.
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

  (defun set-icon-fonts (CODE-FONT-ALIST)
    "Utility to associate unicode points with a chosen font.

CODE-FONT-ALIST is an alist of a font and unicode points to force to use it."
    (mapc (lambda (x)
            (let ((font (car x))
                  (codes (cdr x)))
              (mapc (lambda (code)
                      (set-fontset-font t `(,code . ,code) font))
                    codes)))
          CODE-FONT-ALIST))

  ;; NOTE The icons you see are not the correct icons until this is evaluated
  (set-icon-fonts
   '(("fontawesome"
      ;;              
      #xf07c #xf0c9 #xf0c4 #xf0cb)

     ("all-the-icons"
      ;;    
      #xe907 #xe928)

     ("material"
      ;; 
      #xe192)

     ("github-octicons"
      ;;              
      #xf091 #xf059 #xf076 #xf075)

     ("fileicons"
      ;; 
      #xf016)

     ("Symbola"
      ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
      #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
      ;; 𝔹    𝔇       𝔗
      #x1d539 #x1d507 #x1d517))))

;;;; Font-locks
;;;;; Core

(defun module/display/font-locks ()
  "Enable following font-locks for appropriate modes."

  (defun -add-font-lock-kwds (FONT-LOCK-ALIST)
    "Add unicode font lock replacements.

FONT-LOCK-ALIST is an alist of a regexp and the unicode point to replace with.
Used as: (add-hook 'a-mode-hook (-partial '-add-font-lock-kwds the-alist))"
    (defun -build-font-lock-alist (REGEX-CHAR-PAIR)
      "Compose region for each REGEX-CHAR-PAIR in FONT-LOCK-ALIST."
      `(,(car REGEX-CHAR-PAIR)
        (0 (prog1 ()
             (compose-region
              (match-beginning 1)
              (match-end 1)
              ,(concat "	"
                       (list (cadr REGEX-CHAR-PAIR))))))))
    (font-lock-add-keywords nil (mapcar '-build-font-lock-alist FONT-LOCK-ALIST)))

  (defun add-font-locks (FONT-LOCK-HOOKS-ALIST)
    "Utility to add font lock alist to many hooks.

FONT-LOCK-HOOKS-ALIST is an alist of a font-lock-alist and its desired hooks."
    (mapc (lambda (x)
            (lexical-let ((font-lock-alist (car x))
                          (mode-hooks (cdr x)))
              (mapc (lambda (mode-hook)
                      (add-hook mode-hook
                                (-partial '-add-font-lock-kwds font-lock-alist)))
                    mode-hooks)))
          FONT-LOCK-HOOKS-ALIST))

  (add-font-locks
   `((,fira-font-lock-alist        prog-mode-hook  org-mode-hook)
     (,python-font-lock-alist      python-mode-hook)
     (,emacs-lisp-font-lock-alist  emacs-lisp-mode-hook)
     )))

;;;;; Fira-font-locks

(defconst fira-font-lock-alist
  '(;;;; OPERATORS
    ;;;;; Pipes
    ("\\(<|\\)" #Xe14d) ("\\(<>\\)" #Xe15b) ("\\(<|>\\)" #Xe14e) ("\\(|>\\)" #Xe135)

    ;;;;; Brackets
    ("\\(<\\*\\)" #Xe14b) ("\\(<\\*>\\)" #Xe14c) ("\\(\\*>\\)" #Xe104)
    ("\\(<\\$\\)" #Xe14f) ("\\(<\\$>\\)" #Xe150) ("\\(\\$>\\)" #Xe137)
    ("\\(<\\+\\)" #Xe155) ("\\(<\\+>\\)" #Xe156) ("\\(\\+>\\)" #Xe13a)

    ;;;;; Equality
    ("\\(!=\\)" #Xe10e) ("\\(!==\\)"         #Xe10f) ("\\(=/=\\)" #Xe143)
    ("\\(/=\\)" #Xe12c) ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)"#Xe13d) ("[^!/]\\(==\\)[^>]" #Xe13c)

    ;;;;; Equality Special
    ("\\(||=\\)"  #Xe133) ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"   #Xe166)
    ("\\(\\^=\\)" #Xe136)
    ("\\(=:=\\)"  #Xe13b)

    ;;;;; Comparisons
    ("\\(<=\\)" #Xe141) ("\\(>=\\)" #Xe145)
    ("\\(</\\)" #Xe162) ("\\(</>\\)" #Xe163)

    ;;;;; Shifts
    ("[^-=]\\(>>\\)" #Xe147) ("\\(>>>\\)" #Xe14a)
    ("[^-=]\\(<<\\)" #Xe15c) ("\\(<<<\\)" #Xe15f)

    ;;;;; Dots
    ("\\(\\.-\\)"    #Xe122) ("\\(\\.=\\)" #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;;;;; Hashes
    ("\\(#{\\)"  #Xe119) ("\\(#(\\)"   #Xe11e) ("\\(#_\\)"   #Xe120)
    ("\\(#_(\\)" #Xe121) ("\\(#\\?\\)" #Xe11f) ("\\(#\\[\\)" #Xe11a)

    ;;;; REPEATED CHARACTERS
    ;;;;; 2-Repeats
    ("\\(||\\)" #Xe132)
    ("\\(!!\\)" #Xe10d)
    ("\\(%%\\)" #Xe16a)
    ("\\(&&\\)" #Xe131)

    ;;;;; 2+3-Repeats
    ("\\(##\\)"       #Xe11b) ("\\(###\\)"         #Xe11c) ("\\(####\\)" #Xe11d)
    ("\\(--\\)"       #Xe111) ("\\(---\\)"         #Xe112)
    ("\\({-\\)"       #Xe108) ("\\(-}\\)"          #Xe110)
    ("\\(\\\\\\\\\\)" #Xe106) ("\\(\\\\\\\\\\\\\\)" #Xe107)
    ("\\(\\.\\.\\)"   #Xe124) ("\\(\\.\\.\\.\\)"   #Xe126)
    ("\\(\\+\\+\\)"   #Xe138) ("\\(\\+\\+\\+\\)"   #Xe139)
    ("\\(//\\)"       #Xe12f) ("\\(///\\)"         #Xe130)
    ("\\(::\\)"       #Xe10a) ("\\(:::\\)"         #Xe10b)

    ;;;; ARROWS
    ;;;;; Direct
    ("[^-]\\(->\\)" #Xe114) ("[^=]\\(=>\\)" #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"    #Xe113) ("\\(->>\\)"    #Xe115)
    ("\\(==>\\)"    #Xe13e) ("\\(=>>\\)"    #Xe140)
    ("\\(<--\\)"    #Xe153) ("\\(<<-\\)"    #Xe15d)
    ("\\(<==\\)"    #Xe158) ("\\(<<=\\)"    #Xe15e)
    ("\\(<->\\)"    #Xe154) ("\\(<=>\\)"    #Xe159)

    ;;;;; Branches
    ("\\(-<\\)"  #Xe116) ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)"  #Xe144) ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142) ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146) ("\\(<=<\\)" #Xe15a)

    ;;;;; Squiggly
    ("\\(<~\\)" #Xe160) ("\\(<~~\\)" #Xe161)
    ("\\(~>\\)" #Xe167) ("\\(~~>\\)" #Xe169)
    ("\\(-~\\)" #Xe118) ("\\(~-\\)"  #Xe165)

    ;;;; MISC
    ("\\(www\\)"                   #Xe100)
    ("\\(<!--\\)"                  #Xe151)
    ("\\(~@\\)"                    #Xe164)
    ("[^<]\\(~~\\)"                #Xe168)
    ("\\(\\?=\\)"                  #Xe127)
    ("[^=]\\(:=\\)"                #Xe10c)
    ("\\(/>\\)"                    #Xe12e)
    ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
    ("[^:=]\\(:\\)[^:=]"           #Xe16c)
    ("\\(<=\\)"                    #Xe157)
  ))

;;;;; Language-font-locks

(defconst emacs-lisp-font-lock-alist
  ;; Outlines not using * so better overlap with in-the-wild packages.
  ;; Intentionally not requiring BOL for eg. fira config modularization
  '(("\\(^;;;\\)"                   ?■)
    ("\\(^;;;;\\)"                  ?○)
    ("\\(^;;;;;\\)"                 ?✸)
    ("\\(^;;;;;;\\)"                ?✿)))

(defconst python-font-lock-alist
  ;; Outlines
  '(("\\(^# \\*\\)[ \t\n]"          ?■)
    ("\\(^# \\*\\*\\)[ \t\n]"       ?○)
    ("\\(^# \\*\\*\\*\\)[ \t\n]"    ?✸)
    ("\\(^# \\*\\*\\*\\*\\)[^\\*]"  ?✿)))

;;;; Prettify-symbols

(defun module/display/prettify-symbols ()
  "Visually replace text with unicode.

Ivy keybinding has 'SPC i u' for consel-unicode-char for exploring options."

  (setq pretty-options
        (-flatten
         (prettify-utils-generate
          ;;;;; Functional
          (:lambda      "λ") (:def         "ƒ")
          (:composition "∘")

          ;;;;; Types
          (:null        "∅") (:true        "𝕋") (:false       "𝔽")
          (:int         "ℤ") (:float       "ℝ")
          (:str         "𝕊") (:bool        "𝔹")

          ;;;;; Flow
          (:in          "∈") (:not-in      "∉")
          (:return     "⟼") (:yield      "⟻")
          (:not         "￢")
          (:for         "∀")
          (:and         "∧")
          (:or          "∨")

          ;;;;; Other
          (:tuple       "⨂")
          (:pipe        "")
          )))

  (defun get-pretty-pairs (KWDS)
    "Utility to build an alist for prettify-symbols-alist from components.

KWDS is a plist of pretty option and the text to be replaced for it."
    (-non-nil
     (--map (when-let (major-mode-sym (plist-get KWDS it))
             `(,major-mode-sym
               ,(plist-get pretty-options it)))
           pretty-options)))

  (setq python-pretty-pairs
        (append
         (get-pretty-pairs
          '(:lambda "lambda" :def "def"
                    :null "None" :true "True" :false "False"
                    :int "int" :float "float" :str "str" :bool "bool"
                    :not "not" :for "for" :in "in" :not-in "not in"
                    :return "return" :yield "yield"
                    :tuple "Tuple"
                    :pipe "tz-pipe"
                    :and "and" :or "or"
                    ))
         (prettify-utils-generate
          ;; Mypy Stuff
          ("Dict"     "𝔇") ("List"     "ℒ")
          ("Callable" "ℱ") ("Mapping"  "ℳ") ("Iterable" "𝔗")
          ("Union"    "⋃") ("Any"      "❔")))
        )

  (defun set-pretty-pairs (HOOK-PAIRS-ALIST)
    "Utility to set pretty pairs for many modes.

MODE-HOOK-PAIRS-ALIST is an alist of the mode hoook and its pretty pairs."
    (mapc (lambda (x)
            (lexical-let ((pretty-pairs (cadr x)))
              (add-hook (car x)
                        (lambda ()
                          (setq prettify-symbols-alist pretty-pairs)))))
          HOOK-PAIRS-ALIST))

  (set-pretty-pairs `((python-mode-hook ,python-pretty-pairs)))

  (global-prettify-symbols-mode 1)
  (global-pretty-mode t)

  ;; Activate pretty groups
  (pretty-activate-groups
   '(:arithmetic-nary :greek))

  ;; Deactivate pretty groups conflicting with Fira Code ligatures
  (pretty-deactivate-groups  ; Replaced by Fira Code
   '(:equality
     :ordering
     :ordering-double
     :ordering-triple
     :arrows
     :arrows-twoheaded
     :punctuation
     :logic
     :sets
     :sub-and-superscripts)))
