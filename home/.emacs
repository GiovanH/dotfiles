(add-to-list 'load-path "~/.emacs.d/lisp/")

;(add-to-list 'load-path "~/src/emacs-load-time")
;(require 'emacs-load-time)

(setq tetris-score-file "~/.emacs.d/tetris-scores")
(autoload 'tetris "tetris" "tetris major mode" t)

;Major modes

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

;Themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

(load-theme 'zenburn t)
;(load-theme 'solarized-dark t)

;Commands

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

(add-hook 'after-save-hook #'delete-trailing-whitespace)

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


(show-paren-mode 1)                            ; Highlight parenthesis pairs

(if (not (window-system))
    (menu-bar-mode -1))

;(toggle-scroll-bar -1)
(tool-bar-mode -1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
