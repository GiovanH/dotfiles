;Package management
(add-to-list 'load-path "~/.emacs.d/lisp/")

; (add-to-list 'load-path "~/src/emacs-load-time")
; (require 'emacs-load-time)

;Custom
(custom-set-variables
 '(package-selected-packages
   '(ztree visual-regexp-steroids undo-fu anaconda-mode which-key cheatsheet magit helpful evil-collection)))
(custom-set-faces )

(require 'package)
; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
; (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
; (package-install-selected-packages)
(package-initialize) ; require packages

;Major modes
(message "Loading modes")

(autoload 'terraform-mode "terraform-mode" "terraform major mode" t)
(add-to-list 'auto-mode-alist '("\\.hcl\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))

(autoload 'jinja2-mode "jinja2-mode" "jinja2 major mode" t)
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

(autoload 'yaml-mode "yaml-mode" "YAML major mode" t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(autoload 'edit-indirect "edit-indirect" "Indirect code editing" t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-asymmetric-header t) ; don't use symmetric markdown header

(autoload 'rainbow-mode "rainbow-mode" "rainbow-mode; displays colors inline" t)

; (require 'powerline)
; (powerline-default-theme)

(setq tetris-score-file "~/.emacs.d/tetris-scores")
(autoload 'tetris "tetris" "tetris major mode" t)

(require 'which-key)
(which-key-mode t)

;load ztree and things that hook it (evil) when loading ztree-dir
(autoload #'ztree-dir "ztree" nil t)

(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

;ido

(require 'ido)
(ido-mode t)

;;ido M-x
; (global-set-key
;  "\M-x"
;  (lambda ()
;    (interactive)
;    (call-interactively
;     (intern
;      (ido-completing-read
;       "M-x "
;       (all-completions "" obarray 'commandp))))))

;Themes
; (message "Loading themes")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

(setq zenburn-override-colors-alist
      '(
        ; ("zenburn-bg+05" . "#282828")
        ; ("zenburn-bg+1"  . "#2F2F2F")
        ; ("zenburn-bg+2"  . "##282923")
        ; ("zenburn-bg+3"  . "#4F4F4F")
        )
      )
(load-theme 'zenburn t)
;(load-theme 'solarized-dark t)

;Commands
; (message "Loading settings")

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; Broken
;(defun toggle-fundamental-mode ()
;  (interactive)
;  (cond ((not (eq major-mode "fundamental-mode")) (fundamental-mode))
;        (t (normal-mode))))

;Keys
;; Unbind 'C-x f'
(global-unset-key "\C-xf")

;(global-set-key [f12] 'indent-buffer)
(global-set-key (kbd "C-<f10>") 'menu-bar-open)
;(global-set-key (kbd "<tab>") 'indent-region)
(global-set-key (kbd "C-c r") 'replace-regexp)

;Hooks

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;Making C-x k end an emacsclient session
(add-hook 'server-switch-hook
	  (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))

;Configuration

(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)            ; Use spaces instead of tabs
(setq next-line-add-newlines t)                ; Add newline when at buffer end
(setq undo-limit 100000)                       ; Increase number of undo
(setq-default buffer-file-coding-system 'utf-8-unix) ; Correct line endings
; (setq package-check-signature nil)

(show-paren-mode 1)                            ; Highlight parenthesis pairs

(if (not (window-system))
    (menu-bar-mode -1))

;(toggle-scroll-bar -1)
;not defined in modern emacs
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;Helpful

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point) ;; Lookup the current symbol at point.
(global-set-key (kbd "C-h F") #'helpful-function)   ;; Look up *F*unctions (excludes macros).
(global-set-key (kbd "C-h C") #'helpful-command)    ;; Look up *C*ommands.

; (message ".emacs'd.")