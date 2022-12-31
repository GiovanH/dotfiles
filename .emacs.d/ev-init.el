(message "evil grows in the dark")

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
  :config
  (evil-mode 1)
  ;(add-hook 'with-editor-mode-hook 'evil-insert-state)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(message "evil grows in cracks and holes")

(add-to-list 'load-path (car (file-expand-wildcards "~/.emacs.d/evil-collection-*")))
(require 'evil-collection)

;; Manually enable evil-collection extensions for modes in this list

(defun evil-collection-loadmode (mode &optional altmode)
  (let ((setupfn (concat "evil-collection-" mode "-setup"))
        (elfile (car (file-expand-wildcards (concat "~/.emacs.d/evil-collection-*/modes/" mode "/evil-collection-" mode ".el")))))
    ;; (message "will autoload %s %s with %s" mode setupfn elfile)
    (autoload (intern setupfn) elfile)
    (eval-after-load (intern (or altmode mode)) (funcall (intern setupfn)))
))

(dolist
  (mode '("dired" "buff-menu" "compile" "markdown-mode" "smerge" "info" "which-key" "tar-mode"))
  (condition-case nil
      (evil-collection-loadmode mode)
  (wrong-type-argument (message (format "Failed evil loading %s" mode)))))

;; (let* ((mode "ztree")
;;        (setupfn "evil-collection-ztree-setup")
;;        (elfile (car (file-expand-wildcards (concat "~/.emacs.d/evil-collection-*/modes/" mode "/evil-collection-" mode ".el")))))
;;   (message "will autoload %s %s with %s" mode setupfn elfile)
;;   (autoload (intern setupfn) elfile)
;;   (eval-after-load 'ztree-dir (funcall (intern setupfn))))

(condition-case nil
    (evil-collection-loadmode "ztree" "ztree-dir")
  (wrong-type-argument (message "Failed evil-loading ztree")))

(use-package magit
  :commands (magit)
  :config (evil-collection-loadmode "magit")
          ;;(add-hook 'with-editor-mode-hook 'evil-insert-state)
  )

(message "and lives in people's minds")

(define-key evil-normal-state-map "x" 'execute-extended-command) ;; N x = M-x
(define-key evil-normal-state-map "q" 'quit-window) ;; restore emacs q (evil macros don't work)
(define-key evil-normal-state-map [escape] nil) ; restore M- behavior in normal mode
(define-key evil-visual-state-map (kbd "<backtab>") 'evil-shift-left)

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

(dolist
  (mode '(;buffer-menu-mode
          artist-mode
          picture-mode
          floodit-mode
          speedbar-mode
          ))
  (add-to-list 'evil-emacs-state-modes mode))

(add-hook 'speedbar-mode-hook (
    lambda ()
    (evil-emacs-state)
))

(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  (load-file "~/.emacs.d/ev-init.el"))

