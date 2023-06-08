;; Emacs configuration for optional packages not installed by default.
;; This requires you to install files manually, or run ./setmeup.sh first

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

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
      (add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
      (setq package-selected-packages '(
        annalist ansi
        bind-key
        cl-generic company
        counsel
        dash
        elisp-refs epl evil evil-collection
        f fiplr flycheck
        git-modes goto-chg grizzl
        hyperbole
        ivy
        lv
        lsp-mode lsp-ui lsp-ivy ;; lsp-treemacs
        ;; dap-mode
        magit
        markdown-mode
        popup
        s seq swiper
        treemacs treemacs-evil treemacs-magit
        undo-fu use-package
        with-editor
        pydoc
        x509-mode
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
  )

;;# Flycheck
(use-package flycheck
  :commands (flycheck-mode)
  :config
  (flycheck-define-checker python-pycodestyle
    "A Python syntax and style checker using pycodestyle (former pep8)."
    :command ("python3" "-m" "pycodestyle" "--max-line-length=120" "--ignore=E128" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-pycodestyle)
  (setq flycheck-error-list-format
  `[("File" 8)
      ("Line" 5 flycheck-error-list-entry-< :right-align t)
      ("Col" 3 nil :right-align t)
      ("Level" 8 flycheck-error-list-entry-level-<)
      ("ID" 6 t)
      (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]))

;; (use-package flymake-shellcheck
;;   :commands flymake-shellcheck-load
;;   :init
;;   (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package sunrise
  :commands (sunrise sunrise-cd)
  :config (setq sunrise-cursor-follows-mouse nil))

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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (XXX-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package py-autopep8
  ;; :hook python-mode
  :commands py-autopep8
  :config
  (setq py-autopep8-options '("--aggressive" "--ignore=E302,E305,E226,E128,W50,E722" "--max-line-length=120")))

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
(add-to-list 'load-path "~/.emacs.d/swiper-0.13.0/")
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

(require 'which-key)
;;(which-key-setup-minibuffer)
(setq which-key-show-early-on-C-h t)
(which-key-mode t)

;; Golden ratio
(autoload 'golden-ratio "golden-ratio" "Golden ratio" t)
(setq golden-ratio-auto-scale t)
(global-set-key (kbd "C-x g") 'golden-ratio)

(use-package crux
  :bind (("C-c e" . crux-eval-and-replace)
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
     ("h2"    "^## \\(.*\\)$" 1)
     ("h3"    "^### \\(.*\\)$" 1)
     ("h4"    "^#### \\(.*\\)$" 1)
     ("h5"    "^##### \\(.*\\)$" 1)
     ("h6"    "^###### \\(.*\\)$" 1)
     ("fn"    "^\\[\\^\\(.*\\)\\]" 1)
))
  (setq-local imenu-generic-expression imenu-generic-expression-markdown)))

(autoload 'hcl-mode "hcl-mode" "hcl-mode; local major mode for hcl files" t)
(add-to-list 'auto-mode-alist '("\\.hcl\\'" . hcl-mode))

;;(require 'dockerfile-mode)
;;(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
(use-package dockerfile-mode
  :mode (("Dockerfile'" . dockerfile-mode)))

(use-package mustache-mode
  :mode (("\\.tcl'" . mustache-mode)))

;; org-mode is default mode for txt files
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
;;(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

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

(setq tetris-score-file "~/.emacs.d/tetris-scores")
(autoload 'tetris "tetris2" "tetris major mode" t)

