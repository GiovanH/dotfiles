(message "evil grows in the dark")

; (add-to-list 'load-path "~/.emacs.d/evil/")

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-overriding-maps nil)
  (setq evil-collection-want-unimpaired-p nil)
  (setq global-evil-collection-unimpaired-mode nil)
  :config
  (evil-mode 1))

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

(setq evil-undo-system 'undo-fu)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
