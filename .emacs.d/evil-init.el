(message "evil grows in the dark")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq zenburn-use-variable-pitch t)
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)

(unless window-system
  (add-to-list 'zenburn-override-colors-alist
    '("zenburn-bg"       . "unspecified-bg")
))
(load-theme 'zenburn t)
(setq zenburn-override-colors-alist
  '(
    ("zenburn-bg-1"       . "#2b2b2b") ;Active mode line
    ("zenburn-bg-05"      . "#383838") ;Inactive mode line
   )
)
(add-hook 'highlight-indentation-mode-hook (
    lambda ()
    (set-face-background 'highlight-indentation-face "#383838")
    (set-face-background 'highlight-indentation-current-column-face "#2b2b2b")
))

(setq highlight-indentation-blank-lines t)
(autoload 'highlight-indentation-mode "highlight-indentation" "" t)



; (add-to-list 'load-path "~/.emacs.d/evil/")

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-overriding-maps nil)
  (setq evil-collection-want-unimpaired-p nil)
  (setq global-evil-collection-unimpaired-mode nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

; (setq evil-want-integration t)
; (setq evil-want-keybinding nil)
; (setq evil-overriding-maps nil)
; (setq evil-collection-want-unimpaired-p nil)
; (setq global-evil-collection-unimpaired-mode nil)

; (require 'evil)

(message "where the sun it never shines")

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

; (require 'evil-collection)
; (evil-collection-init)

; (add-to-list 'load-path "~/.emacs.d/evil/")

(message "evil grows in cracks and holes and lives in peoples' minds")

; (require 'powerline-evil)
; (powerline-evil-vim-theme)

(define-key evil-normal-state-map "x" 'execute-extended-command) ;; N x = M-x
(define-key evil-normal-state-map [escape] nil) ; restore M- behavior in normal mode

;not working
;(define-key evil-visual-state-map (kbd "<tab>") indent-region)

(message "where the sun it never shines")

