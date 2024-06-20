;; Emacs configuration for optional packages not installed by default.

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; (if (fboundp 'advice-add) (progn
;;   (defun my-advice-silence-messages (func &rest args)
;;   "Invoke FUNC with ARGS, silencing all messages. This is an `:around' advice for many different functions."
;;   (cl-letf (((symbol-function #'msg) #'ignore)
;;             ((symbol-function #'message) #'ignore))
;;       (apply func args)))
;;   (dolist (func '(define-minor-mode do-after-load-evaluation))
;;   (advice-add func :around #'my-advice-silence-messages))
;; ))
(message "never go becret")

;;=====================================================================
;;# backports

(if (version< emacs-version "24.4") (progn
  (message "oh no! i am old")
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
    FILE is normally a feature name, but it can also be a file name,
    in case that file does not provide any feature.  See `eval-after-load'
    for more details about the different forms of FILE and their semantics."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body)))


  (defalias 'format-message
    ;(if (fboundp 'format-message) 'format-message
        ;; for Emacs < 25, and XEmacs, don't worry about quote translation.
        'format
    ;)
  )

  ;; backport emacs ebb99847285bca912e04f79dd3d9dcc84769ccf6
  (require 'cl)
  (if (not (fboundp 'c=)) (defalias 'c= (symbol-function '=)))
  (defun = (num1 &rest nums) (every (lambda (num2) (c= num1 num2)) nums))
  (if (not (fboundp 'c<=)) (defalias 'c<= (symbol-function '<=)))
  (defun <= (&rest nums) (every #'c<= nums (rest nums)))
  (if (not (fboundp 'c<)) (defalias 'c< (symbol-function '<)))
  (defun < (&rest nums) (every #'c< nums (rest nums)))
  (if (not (fboundp 'c>=)) (defalias 'c>= (symbol-function '>=)))
  (defun >= (&rest nums) (every #'c>= nums (rest nums)))
  (if (not (fboundp 'c>)) (defalias 'c> (symbol-function '>)))
  (defun > (&rest nums) (every #'c> nums (rest nums)))
))


;; workaround emacs 32c6732d16385f242b1109517f25e9aefd6caa5c

;;(fset 'original-define-obsolete-variable-alias (symbol-function 'define-obsolete-variable-alias))
;;(defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
;; (original-define-obsolete-variable-alias obsolete-name current-name (or when "unknown") docstring))


;;=====================================================================
;;# Package management
(defun my-package-update ()
  (interactive)
  (let (;; Workarounds, enable as needed
        ;;(debug-on-error t)
        ;;(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
        ;;(package-check-signature nil)
       )
      ;(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    ;; (add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

      (setq package-selected-packages '(
        annalist ansi
        bind-key
        cl-generic company
        counsel
        dash dired-du dired-quick-sort
        elisp-refs epl evil evil-collection
        f fiplr flycheck
        git-modes goto-chg grizzl
        hyperbole hydra
        ivy
        json-mode json-navigator
        lv
        lsp-mode lsp-ui lsp-ivy ;; lsp-treemacs
        ;; dap-mode
        magit
        markdown-mode
		;; org-lazy-babel
        popup
        s seq swiper sunrise
        treemacs treemacs-evil treemacs-magit
        transient
        undo-fu use-package
        with-editor which-key
        pydoc projectile
        x509-mode
        zenburn-theme
      ))
      (package-refresh-contents)
      (package-install-selected-packages)))


(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'package)
(package-initialize) ; Load lisp packages and activate them
(unless (package-installed-p 'use-package)
  (my-package-update))
(require 'pkg-info)

;;=====================================================================
;;# Software

;; Magit
(use-package magit
  :commands (magit magit-list-repositories)
  :config (setq magit-diff-refine-hunk nil)
          (setq magit-repository-directories `((,(expand-file-name "~/gits/") . 1)))
          (setq magit-revert-buffers 1)
          (setq magit-diff-refine-hunk nil)

  )

;; (use-package window-purpose)

;; Keyquiz

;; TODO: evil-normal-state evil-open-fold, evil-close-fold

(defun my-key-quiz ()
  "Personal key quiz."
  (interactive)
  (require 'key-quiz)

  (defun keypairs-from-functions (fns keymap)
    (defun flatten (list-of-lists)
      (apply #'append list-of-lists))

    (flatten (mapcar
      (lambda (fn)
        (let ((pairs
          (mapcar
            (lambda (key) (cons key (symbol-name fn)))
            (mapcar #'key-description (where-is-internal fn keymap)))
          ))
          (unless (>= (length pairs) 1)
            (error "Could not resolve all: %s %s %s " fn pairs (length pairs)))
          pairs))
      fns)))
  (key-quiz nil
    ;; Construct (key . command) list
    (append
      (keypairs-from-functions
        '(
           find-file
           save-buffer
           save-some-buffers
           describe-keymap
           goto-char
           goto-line
           mark-defun
           indent-rigidly
           transpose-lines
           upcase-region
           downcase-region
           shell-command-on-region
           back-to-indentation
           eval-expression
           recenter-top-bottom
           downcase-word
           upcase-word
           capitalize-word
           dabbrev-expand
           ;; User
           switch-to-minibuffer
           ffap
           py-eval-region
           swiper
           counsel-imenu
           crux-eval-and-replace
           crux-rename-file-and-buffer
           avy-goto-char
           ace-window
           kmacro-start-macro-or-insert-counter
           kmacro-end-or-call-macro
           recenter-top-bottom
        ) global-map)
      (keypairs-from-functions
        '(
           evil-forward-paragraph
           evil-backward-paragraph
           evil-scroll-line-down
           evil-scroll-line-up
           evil-goto-mark-line
           evil-window-middle
           evil-window-bottom
           evil-forward-WORD-begin
        ) evil-motion-state-map)
      (keypairs-from-functions
        '(
           evil-open-fold
           evil-close-fold
           evil-open-above
           evil-open-below
           ;; evil-replace-state
           evil-join
           evil-set-marker
           ;; evil-execute-in-emacs-state
        ) evil-normal-state-map)
      (keypairs-from-functions
        '(
           evil-quoted-insert
           evil-paste-last-insertion
           evil-shift-left-line
           evil-shift-right-line
           evil-insert-digraph
        ) evil-insert-state-map)
      (keypairs-from-functions
        '(
           dired-hide-details-mode
           dired-maybe-insert-subdir
        ) dired-mode-map)
    )
  )
)

;;# Flycheck
(use-package flycheck
  :commands (flycheck-mode)
  :config
  (flycheck-define-checker python-pycodestyle
    "A Python syntax and style checker using pycodestyle (former pep8)."
    :command ("python3" "-m" "pycodestyle" "--max-line-length=120" "--ignore=E128" "-")
    :standard-input t
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes python-mode)
  (flycheck-define-checker python-mypy "Python type checking with MyPy"
    :command ("mypy"
              "--ignore-missing-imports" "--fast-parser"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-pycodestyle)
  ;; (add-to-list 'flycheck-checkers 'python-mypy)
  ;; (flycheck-add-next-checker 'python-pycodestyle 'python-mypy t)
  (setq flycheck-error-list-format
  `[("File" 8)
      ("Line" 5 flycheck-error-list-entry-< :right-align t)
      ("Col" 3 nil :right-align t)
      ("Level" 8 flycheck-error-list-entry-level-<)
      ("ID" 6 t)
      (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(defun shellcheck-ediff ()
  (interactive)
  (let* ((tmpbuffer (make-temp-name "patch")))
    (call-shell-region nil nil "shellcheck -f diff -" nil tmpbuffer)
    (ediff-patch-file 2 tmpbuffer)
  )
)

(use-package efar
  :commands (efar)
  :config
  (bind-key "<mouse-4>" #'efar-do-move-up 'efar-mode-map)
  (bind-key "<mouse-5>" #'efar-do-move-down 'efar-mode-map)
  )

;; (use-package sunrise
;;   :commands (sunrise sunrise-cd)
;;   :config (setq sunrise-cursor-follows-mouse nil))

(use-package projectile
  :commands (projectile-mode)
  :bind-keymap (("C-c p" . projectile-command-map))
  :config (require 'ivy)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-mode-line-function
    #'(lambda () (format " Proj[%s]" (projectile-project-name))))
  (add-hook 'projectile-mode-hook (lambda ()
     (setq-local projectile-enable-caching t)
  ))
)
;;(projectile-mode +1)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;=====================================================================
;;# Integration

(use-package py-autopep8
  ;; :hook python-mode
  :commands py-autopep8
  :config
  (setq py-autopep8-options '("--aggressive" "--ignore=E302,E305,E226,E128,W50,E722" "--max-line-length=120")))


;; LSP support
(use-package eglot
  :commands eglot
  :config
  (use-package flycheck-eglot
    :after (flycheck eglot)
    :config
    (setq flycheck-eglot-exclusive nil)
    (global-flycheck-eglot-mode 1))
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives
                                  '(("python3" "/home/sgiovan/workspace/dev-vscode-python/build/extensions/charliermarsh.ruff-2023.50.0/extension/bundled/tool/server.py"
                                      :initializationOptions
                                      ())
                                  ("phewls" "--fast")))))
  (add-to-list 'flycheck-checkers 'eglot-check)
  (flycheck-add-next-checker 'python-pycodestyle 'eglot-check)
)

;;(add-hook 'python-mode-hook (lambda ()
;;  (require 'py-autopep8)
;;  (setq py-autopep8-options '("--aggressive" "--ignore=E302,E305,E226,E128,W50,E722" "--max-line-length=120"))
;;  ; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;))

(use-package tramp-term
  :commands tramp-term)

;; eir

(autoload #'eir-eval-in-ielm "eval-in-repl-ielm" nil t)
(bind-key "<C-return>" #'eir-eval-in-ielm 'emacs-lisp-mode-map)
(bind-key "<C-return>" #'eir-eval-in-ielm 'lisp-interaction-mode-map)

;; (autoload #'eir-eval-in-ruby "eval-in-repl-ruby" nil t)
;; (define-key ruby-mode-map (kbd "<C-return>") 'eir-eval-in-ruby)

(autoload #'eir-eval-in-shell "eval-in-repl-shell" nil t)
(add-hook 'sh-mode-hook
          #'(lambda() (local-set-key (kbd "C-<return>") 'eir-eval-in-shell)))

(autoload #'eir-eval-in-python "eval-in-repl-python" nil t)
(add-hook 'python-mode-hook
          #'(lambda ()
              (local-set-key (kbd "<C-return>") 'eir-eval-in-python)
              (bind-key "<C-return>" #'eir-eval-in-python 'python-mode-map)))

;;=====================================================================
;;# Tweaks & Features

;; string-inflection
(use-package string-inflection)

;; (require 'page-break-lines)
;; (global-page-break-lines-mode 1)

(require 'org-lazy-babel)

;; ace-window
(use-package ace-window
  :bind (("M-o" . 'ace-window))
  :config
  (setq aw-keys '(?j ?k ?l ?a ?s ?d ?f ?g ?h)))

(use-package treemacs
  :commands (treemacs)
  :config
  (require 'treemacs-evil)
  (require 'treemacs-projectile))

(use-package treemacs-magit
  :commands (treemacs-magit))

(use-package treemacs
  :commands (treemacs))

(setq tetris-score-file "~/.emacs.d/tetris-scores")
(autoload 'tetris "tetris2" "tetris major mode" t)

(autoload 'floodit "floodit" "floodit major mode" t)

;; Hideshow
;; (defun +data-hideshow-forward-sexp (arg)
;;   (let ((start (current-indentation)))
;;     (forward-line)
;;     (unless (= start (current-indentation))
;;       (require 'evil-indent-plus)
;;       (let ((range (evil-indent-plus--same-indent-range)))
;;         (goto-char (cadr range))
;;         (end-of-line)))))
;; (add-to-list 'hs-special-modes-alist
;;  '(yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>" "" "#" +data-hideshow-forward-sexp nil))

;; (add-hook 'python-mode-hook (lambda ()
;;   (defun fate/python-hideshow-forward-sexp-function (arg)
;;     "Python specific `forward-sexp' function for `hs-minor-mode'.
;;     Argument ARG is ignored."
;;     arg  ; Shut up, byte compiler.
;;     (python-nav-end-of-block))
;;   "Replace `hs-special-modes-alist' for `python-mode'."
;;   (let
;;     ((python-mode-hs-info
;;        '(python-mode
;;           "\\s-*\\_<\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\|with\\)\\_>" "" "#"
;;           fate/python-hideshow-forward-sexp-function
;;           nil)))
;;     (setq hs-special-modes-alist (cl-remove-if #'(lambda (x) (eq (car x) 'python-mode)) hs-special-modes-alist))
;;     (add-to-list 'hs-special-modes-alist python-mode-hs-info)
;;     (hs-grok-mode-type))
;; ))

(defun company-initialize-and-complete ()
  "Lazy-load company and immedately try completion."
  (interactive)
  (company-mode 1)
  (company-complete))

(use-package company
  :bind (("C-c TAB" . company-initialize-and-complete)
         ("C-<tab>" . company-initialize-and-complete)
         :map company-mode-map
         ("C-c TAB" . company-complete)
         ("C-<tab>" . company-complete))
  :init  (require 'company-flyspell))

(add-to-list 'load-path "~/.emacs.d/elpa/hyperbole-8.0.0/")
(use-package hyperbole
  :bind (("M-<return>"  . hyperbole-mode)
        ;; ("C-h h"       . hyperbole-mode)
         ("S-<mouse-2>" . hyperbole-mode))
  :config
    (defun hui:actype (&optional default-actype prompt)
    "Using optional DEFAULT-ACTYPE, PROMPT for and return a button action type.
    DEFAULT-ACTYPE may be a valid symbol or symbol name."
    (when (and default-actype (symbolp default-actype))
        (setq default-actype (symbol-name default-actype)
        default-actype (actype:def-symbol default-actype)
        default-actype (when default-actype (symbol-name default-actype))))
    (if (or (null default-actype) (stringp default-actype))
        (let ((actype-name
            (hargs:read-match (or prompt "Button's action type: ")
                    (mapcar #'list (htype:names 'actypes))
                    nil t default-actype 'actype)))
        (or (actype:def-symbol actype-name) (intern actype-name)))
      (hypb:error "(actype): Invalid default action type received")))
)


;; Ivy/counsel/swiper
(add-to-list 'load-path
  (car (file-expand-wildcards "~/.emacs.d/elpa/swiper-*/")))

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :config (setq ivy-use-virtual-buffers t)
          ;(setq enable-recursive-minibuffers t)
          (bind-key "C-r" #'counsel-minibuffer-history 'minibuffer-local-map)
  :commands (ivy-mode))

(use-package counsel
  :bind (("C-c i" . counsel-imenu))
  :config (bind-key "C-r" #'counsel-minibuffer-history 'minibuffer-local-map)
  :commands (ivy-mode))

(use-package swiper
  :commands swiper
  :bind (("C-s" . swiper)))

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-show-early-on-C-h t)
    (add-to-list
    'which-key-replacement-alist
    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
    )
  )

;; Golden ratio
(autoload 'golden-ratio "golden-ratio" "Golden ratio" t)
(setq golden-ratio-auto-scale t)
(global-set-key (kbd "C-x g") 'golden-ratio)

(use-package crux
  :bind (("C-c C-e" . crux-eval-and-replace)
         ;("C-c o" . crux-open-with)
         ("C-c C-r" . crux-rename-file-and-buffer)
         ("C-x 4 t" . crux-transpose-windows)))

(require 'wrap-region)
(wrap-region-add-wrappers
 '(("{-" "-}" "#")
   ("/* " " */" "#" (java-mode javascript-mode css-mode))
   ("`" "`" nil (markdown-mode ruby-mode))))

(use-package avy
  :bind (("C-:" . 'avy-goto-char)))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package highlight-indentation
  :commands (highlight-indentation-mode)
  :config (setq highlight-indentation-blank-lines t))

(use-package sr-speedbar
  :commands (sr-speedbar-open)
  :config
    (setq sr-speedbar-right-side nil)
    (setq speedbar-show-unknown-files t))

;;(autoload 'sr-speedbar-open "sr-speedbar" "Same-frame speedbar" t)
;;(defalias 'sr-speedbar 'sr-speedbar-open)

;;load ztree and things that hook it (evil) when loading ztree-dir
(autoload #'ztree-dir "ztree" nil t)

(use-package undo-fu
  :config
  (global-set-key (kbd "C-x u") 'undo-fu-only-undo)
  (global-set-key (kbd "C-r") 'undo-fu-only-redo))


;;=====================================================================
;;# Language support

(message "Loading modes")

(autoload 'markdown-mode "markdown-mode" "markdown-mode; local major mode" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq markdown-asymmetric-header t) ; don't use symmetric markdown header

(add-hook 'markdown-mode-hook (lambda ()
(setq-local imenu-generic-expression-markdown
    '(;; ("title" "^\\(.*\\)[\n]=+$" 1)
      ;; ("h2-"   "^\\(.*\\)[\n]-+$" 1)
      ;; ("h1"    "^# \\(.*\\)$" 1)
      ("h2" "^## \\(.*\\)$" 1)
      ("h3" "^### \\(.*\\)$" 1)
      ("h4" "^#### \\(.*\\)$" 1)
      ("h5" "^##### \\(.*\\)$" 1)
      ("h6" "^###### \\(.*\\)$" 1)
      ("fn" "^\\[\\^\\(.*\\)\\]" 1)
  ))
  (setq-local imenu-generic-expression imenu-generic-expression-markdown)))

;;(require 'dockerfile-mode)
;;(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
(use-package dockerfile-mode
  :mode (("Dockerfile'" . dockerfile-mode)))

(use-package json-mode
  :mode (("\\.json" . json-mode))
  ;;:config (add-hook 'json-mode-hook (lambda () (json-par-mode 1)))
)

(use-package mustache-mode
  :mode (("\\.tcl'" . mustache-mode)))

(use-package jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

;;(add-hook 'python-mode-hook (lambda ()
;;  (require 'py-autopep8)
;;  (setq py-autopep8-options '("--aggressive" "--ignore=E302,E305,E226,E128,W50,E722" "--max-line-length=120"))
;;  ; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;))

(use-package jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package yaml-mode
  :mode (("\\.ya?ml\\(\\'\\|\\.\\)" . yaml-mode))
  :config
        (require 'yaml-path)
        (bind-key "C-c C-p" #'yaml-path/path 'yaml-mode-map)
)

(defun outline-hook-yaml ()
  (outline-minor-mode)
  ;;(setq outline-regexp "^ *\\([A-Za-z0-9_-]*: *[>|]?$\\|-\\b\\)")
  ;;(setq outline-regexp "^ *\\([A-Za-z0-9_-]*: *[>|]?$\\|-\\b\\)")
  (setq-local outline-regexp
      (rx
       (seq
    bol
    (group (zero-or-more "  ")
           (or (group
            (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                 (seq "'" (*? (not (in "'" "\n"))) "'")
                 (*? (not (in ":" "\n"))))
             ":"
             (?? (seq
                  (*? " ")
                  (or (seq "&" (one-or-more nonl))
                  (seq ">-")
                  (seq "|"))
                  eol))))
           (group (seq
               "- "
               (+ (not (in ":" "\n")))
               ":"
               (+ nonl)
               eol)))))))
  )

(add-hook 'yaml-mode-hook 'outline-hook-yaml)

;;# Hydras

(use-package hydra)

;;TODO Artist mode QoL
(add-hook 'artist-mode-hook
  (lambda ()
    (transient-define-prefix artist-tools ()
      ;; :transient-suffix     'transient--do-stay
      ;; :transient-non-suffix 'transient--do-warn
      [("p" "pen tool"     artist-select-op-pen-line)
       ("l" "line tool"    artist-select-op-line)
       ("s" "square tool"  artist-select-op-square)
       ("e" "ellipse tool" artist-select-op-ellipse)
      ]
    )
    (local-set-key (kbd "<f1>") #'artist-tools)
    ;; (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
    ;; (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
    ;; (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
    ;; (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
    (local-set-key (kbd "C-z") 'undo)
  ))

(defhydra hydra-flycheck
  (:pre (let ((display-buffer-overriding-action '(display-buffer-below-selected)))
          (flycheck-list-errors))
     ;; :post (quit-windows-on "*Flycheck errors*")
     :hint nil
     :color pink)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("n" flycheck-next-error "Next")
  ("p" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))
(global-set-key (kbd "C-c k") #'hydra-flycheck/body)

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c .") #'hydra-dired/body)
          ))


;;=====================================================================
;;# Evil

;; (if (not (and (boundp 'my-evil-loaded) (fboundp 'evil-mode))) (progn

(message "evil grows in the dark")

;; https://github.com/noctuid/evil-guide

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq zenburn-use-variable-pitch t)
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)
(setq zenburn-override-colors-alist
  '(
    ("zenburn-bg-1"       . "#2b2b2b") ;Active mode line
    ("zenburn-bg-05"      . "#383838") ;Inactive mode line
    ("zenburn-bg"         . "#2f2f2f") ;Background
   )
)

(unless window-system
  (add-to-list 'zenburn-override-colors-alist
    '("zenburn-bg"       . "unspecified-bg") ;Don't use background characters in terminal
))

(load-theme 'zenburn t)
(add-hook 'highlight-indentation-mode-hook (
    lambda ()
    (set-face-background 'highlight-indentation-face "#383838")
    (set-face-background 'highlight-indentation-current-column-face "#2b2b2b")
))

(message "where the sun it never shines")

; (add-to-list 'load-path "~/.emacs.d/evil/")
; (add-to-list 'load-path "~/.emacs.d/site-lisp/evil-1.14.2/")

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-overriding-maps nil)
  ;; (setq evil-search-module 'isearch)
  (setq evil-search-module 'evil-search)
  (setq evil-collection-want-unimpaired-p nil)
  (setq global-evil-collection-unimpaired-mode nil)
  (setq evil-undo-system 'undo-fu)
  ;; (setq evil-ex-substitute-global t)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global
    "u" 'undo-fu-only-undo
    "\C-r" 'undo-fu-only-redo)
  ;; C-g stops highlighting too
  (defadvice keyboard-quit (before evil activate)
    (when (fboundp 'evil-ex-nohighlight)
      (evil-ex-nohighlight)))
)

(use-package evil-visual-mark-mode
  :config
  (evil-visual-mark-mode 1))

(message "evil grows in cracks and holes")

(add-to-list 'load-path (car
  (file-expand-wildcards "~/.emacs.d/elpa/evil-collection-*")))
(require 'evil-collection)

;; Manually enable evil-collection extensions for modes in this list

(defun evil-collection-loadmode (mode &optional altmode)
  (let ((setupfn (concat "evil-collection-" mode "-setup"))
        (elfile (car (file-expand-wildcards
	                   (concat "~/.emacs.d/elpa/evil-collection-*/modes/" mode "/evil-collection-" mode ".el")))))
    (if (not elfile)
        (message "no file to load %s with %s" mode setupfn)
      (autoload (intern setupfn) elfile)
      (eval-after-load (intern (or altmode mode)) (funcall (intern setupfn))))
))

(dolist
  (mode '("dired" "buff-menu" "compile" "markdown-mode" "smerge" "info" "which-key" "tar-mode"))
    (evil-collection-loadmode mode))

(condition-case nil
    (evil-collection-loadmode "ztree" "ztree-dir")
  (wrong-type-argument (message "Failed evil-loading ztree")))

(evil-collection-loadmode "org" "org-mode")

(use-package magit
  :commands (magit)
  :config (evil-collection-loadmode "magit")
          ;;(add-hook 'with-editor-mode-hook 'evil-insert-state)
  )

(message "and lives in people's minds")

(evil-define-key 'normal 'global
 "x" 'execute-extended-command ;; N x = M-x
 "gr" 'revert-buffer
 "q" 'quit-window ;; restore emacs q (evil macros don't work???)
 [escape] nil ; restore M- behavior in normal mode
)

(evil-define-key 'visual 'global
  (kbd "<backtab>") 'evil-shift-left
  (kbd "<tab>") 'indent-region
)

(evil-define-key 'visual org-mode-map
  "*" (lambda () (interactive) (org-emphasize ?*))
  "/" (lambda () (interactive) (org-emphasize ?/))
  "_" (lambda () (interactive) (org-emphasize ?_))
  "=" (lambda () (interactive) (org-emphasize ?=))
  "~" (lambda () (interactive) (org-emphasize ?~))
  "+" (lambda () (interactive) (org-emphasize ?+))
)

(define-key evil-visual-state-map (kbd "<backtab>") 'evil-shift-left)

(use-package dired-preview
  :commands (dired-preview-mode dired-preview-global-mode)
  :config
  (defun my-dired-preview-to-the-right ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-in-side-window)
      (side . right)
      (width . 1)
    ))

  (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)

  ;; Default values for demo purposes
  (setq dired-preview-delay 0.7)
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                "\\|iso\\|epub\\|pdf\\)"))

  ;; Enable `dired-preview-mode' in a given Dired buffer or do it
  ;; globally:
  (dired-preview-global-mode 1)
)


(use-package dired-quick-sort
  :config
  (evil-define-key 'normal 'dired-mode-map
    "S" 'hydra-dired-quick-sort/body)
  )

;; TODO string-inflection operator
;; (evil-define-operator evil-operator-string-inflection (beg end _type)
;;       "Define a new evil operator that cycles symbol casing."
;;       :move-point nil
;;       (interactive "<R>")
;;       (string-inflection-all-cycle)
;;       (setq evil-repeat-info '([?g ?~])))
;; (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)

(defun evil-quick-replace-selection (start end)
  "Quickly open a command to globally replace the current region with a new value"
  (interactive "r") ; operate on region
  (let* ((escaped (buffer-substring-no-properties start end))
         (escaped (replace-regexp-in-string "\\]" (regexp-quote "]")   escaped))
         (escaped (replace-regexp-in-string "\\*" "\\\\*"              escaped))
         (escaped (replace-regexp-in-string "/"   (regexp-quote "\\/") escaped))
         (escaped (replace-regexp-in-string "\n"  (regexp-quote "\\n") escaped)))
   (evil-ex (concat "%s/" escaped "/"))))

(global-set-key (kbd "C-S-h") 'evil-quick-replace-selection) ;; N x = M-x

(defun force-undo-boundary ()
  (evil-end-undo-step)
  (undo-boundary)
  (evil-start-undo-step))

(add-hook 'after-save-hook 'force-undo-boundary)

(evil-ex-define-cmd "q" 'kill-current-buffer)
(evil-ex-define-cmd "wq" 'save-and-kill-this-buffer)
(defun save-and-kill-this-buffer()
  (interactive)
  (save-buffer)
  (kill-current-buffer)
)

(dolist
  (mode '(;buffer-menu-mode
          artist-mode
          picture-mode
          floodit-mode
          speedbar-mode
          ;;json-par-mode
          ))
  (add-to-list 'evil-emacs-state-modes mode))

(add-hook 'speedbar-mode-hook
  (lambda ()
    (evil-emacs-state)))

(add-hook 'artist-mode-init-hook
  (lambda ()
    (evil-emacs-state)))

(setq evil-shift-width 2)
(add-hook 'python-mode-hook (lambda ()
   (setq-local evil-shift-width 4)))

(defhydra hydra-flymake
  (:pre (let ((display-buffer-overriding-action '(display-buffer-below-selected)))
          (flymake-show-buffer-diagnostics))
     ;; :post (quit-windows-on (flymake--diagnostics-buffer-name))
     :hint nil
     :color pink)
  "Errors"
  ;;("f" flycheck-error-list-set-filter "Filter")
  ("n" flymake-goto-next-error "Next")
  ("N" flymake-goto-prev-error "Previous")
  ("c" flymake-start "Check")
  ("s" flymake-mode "Stop flymake-mode")
  ("q" nil))
(define-key evil-normal-state-map (kbd "C-c m") #'hydra-flymake/body)
;(global-set-key (kbd "C-c m") #'hydra-flymake/body)

(defhydra hydra-flycheck
  (:pre (let ((display-buffer-overriding-action '(display-buffer-below-selected)))
          (flycheck-list-errors))
     ;; :post (quit-windows-on "*Flycheck errors*")
     :hint nil
     :color pink)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("n" flycheck-next-error "Next")
  ("p" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))
;; (global-set-key (kbd "C-c k") #'hydra-flycheck/body)
(define-key evil-normal-state-map (kbd "C-c k") #'hydra-flycheck/body)

(setq my-evil-loaded t)
;;))

;;# Misc

;;TODO quick actions transient
;; (transient-define-prefix quick-menu ()
;;   ;; :transient-suffix     'transient--do-stay
;;   ;; :transient-non-suffix 'transient--do-warn
;;    [("c" "open blog content" (lambda () (find-file "~/Documents/blog/content")))
;;   ]
;; )


