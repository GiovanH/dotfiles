;Package management
(add-to-list 'load-path "~/.emacs.d/lisp/")

; (add-to-list 'load-path "~/src/emacs-load-time")
; (require 'emacs-load-time)

;Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-aspell flycheck-inline flycheck-pyflakes flycheck-yamllint flymake-json flymake-sass async flycheck company-ansible company-lua company-nginx company-shell company-terraform company native-complete evil use-package fiplr ztree undo-fu anaconda-mode which-key magit helpful evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
; (setq package-check-signature nil)
(package-initialize) ; require packages
;(package-install-selected-packages)

;Major modes
(message "Loading modes")

;; org-mode is default mode for txt files
;(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . 'org-mode))

;Async
;(autoload 'dired-async-mode "dired-async.el" nil t)
;(dired-async-mode 1)
;(async-bytecomp-package-mode 1)

(autoload 'edit-indirect "edit-indirect" "Indirect code editing" t)

(use-package jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"
              markdown-asymmetric-header t)
  :config (fset 'macro-md-make-embed [evil-normal-state ?^ ?! ?\[ ?\] ?\( escape ?$ ?A ?\) escape])
          (global-set-key (kbd "C-c k") 'macro-md-make-embed))

(setq markdown-asymmetric-header t) ; don't use symmetric markdown header

(use-package rainbow-mode
  :commands (rainbow-mode))
; (autoload 'rainbow-mode "rainbow-mode" "rainbow-mode; displays colors inline" t)

(setq tetris-score-file "~/.emacs.d/tetris-scores")
(autoload 'tetris "tetris" "tetris major mode" t)

(use-package which-key
  :config (which-key-mode t))

;; fuzzy file finder
(use-package fiplr
  :config
  (setq fiplr-root-markers '(".git" ".svn"))
  (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                              (files ("*.jpg" "*.png" "*.zip" "*~"))))
  (global-set-key (kbd "C-c p p") 'fiplr-find-file)
  (global-set-key (kbd "C-x p p") 'fiplr-find-file))

;load ztree and things that hook it (evil) when loading ztree-dir
(autoload #'ztree-dir "ztree" nil t)


;;Helpful

(use-package helpful
  :bind ("C-h f" . helpful-callable)
        ("C-h v" . helpful-variable)
        ("C-c C-d" . helpful-at-point) ;; Lookup the current symbol at point.
        ("C-h F" . helpful-function)   ;; Look up *F*unctions (excludes macros).
        ("C-h C" . helpful-command)    ;; Look up *C*ommands.
        ("C-h k" . helpful-key))

(use-package undo-fu
  :config
  (global-set-key (kbd "C-x u") 'undo-fu-only-undo)
  (global-set-key (kbd "C-r") 'undo-fu-only-redo))

(defun company-initialize-and-complete ()
  (interactive)
  (company-mode 1)
  (company-complete))
(use-package company
  :bind (("C-c TAB" . company-initialize-and-complete)
         :map company-mode-map
         ("C-c TAB" . company-complete)))

;Themes
; (message "Loading themes")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

;; Broken
;(defun toggle-fundamental-mode ()
;  (interactive)
;  (cond ((not (eq major-mode "fundamental-mode")) (fundamental-mode))
;        (t (normal-mode))))

; extra commands

(defun byte-recompile-smart ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs"))

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))
(add-to-list 'command-switch-alist '("diff" . command-line-diff))

(defun command-line-merge (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff-merge-files file1 file2)))
(add-to-list 'command-switch-alist '("merge" . command-line-merge))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;Keys
;; Unbind 'C-x f'
(global-unset-key "\C-xf")

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-x\C-r" 'rgrep)

;Use ctrl+shift+c/v in x11 mode
(global-set-key (kbd "C-S-C") 'kill-ring-save)
(global-set-key (kbd "C-S-V") 'yank)

;(global-set-key [f12] 'indent-buffer)
(global-set-key (kbd "C-<f10>") 'menu-bar-open)
;(global-set-key (kbd "<tab>") 'indent-region)
(global-set-key (kbd "C-c r") 'replace-regexp)

(global-set-key (kbd "<f5>") 'revert-buffer)
;Hooks

;Delete trailing whitespace
(add-hook 'before-save-hook
          (lambda ()
            (unless (eq major-mode 'markdown-mode)
              (delete-trailing-whitespace))))

;When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;Making C-x k end an emacsclient session
(add-hook 'server-switch-hook
	  (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))

;Configuration

(setq x-alt-keysym 'meta)
(setq next-line-add-newlines nil)
(setq fill-column 120)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq delete-trailing-lines t)
(setq next-line-add-newlines nil)
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)
(setq message-log-max 10000)            ; Increase number of undo
(setq undo-limit 10000)                 ; Increase number of undo
(setq vc-follow-symlinks t)
(setq-default buffer-file-coding-system 'utf-8-unix) ; Correct line endings
(setq sentence-end-double-space nil)
(setq diff-switches "-u")               ; default to unified diffs
(setq require-final-newline t)
(setq ls-lisp-dirs-first t)             ;display dirs first in dired
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default frame-title-format (list "%b @Emacs"))
(setq show-paren-delay 0) (show-paren-mode 1)  ; Highlight parenthesis pairs
;(setq zone-timer (run-with-idle-timer 120 t 'zone))
(setq ring-bell-function 'ignore)
(setq visible-bell t)
;(setq-default show-trailing-whitespace t) ;Show stray whitespace.

(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search

;Show info in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)
(setq savehist-save-minibuffer-history +1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

(if (not (window-system))
    (menu-bar-mode -1))

;fix shell completion
(advice-add 'comint-term-environment
            :filter-return (lambda (env) (cons "INSIDE_EMACS" env)))
;(require 'comint)
;(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
;(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;(require 'native-complete)
;(with-eval-after-load 'shell
;  (native-complete-setup-bash))

;not defined in modern emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

; org config

;; Run/highlight code using babel in org-mode
(with-eval-after-load 'org-mode
  (use-package ob-async)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (shell . t)
     ;; Include other languages here...
     ))
  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil))

; (message ".emacs'd.")
