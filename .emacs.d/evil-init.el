(load-file "~/dotfiles/home/.emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(unless window-system 
  (setq zenburn-override-colors-alist
  '(
    ("zenburn-bg"       . "unspecified-bg")
   )
))
(load-theme 'zenburn t)

(message "evil grows in the dark")

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

(setq evil-collection-mode-list '(
  calc 
  comint
  company
  ediff
  flycheck
  flymake
  org
  which-key
  woman
  yaml-mode
  (custom cus-edit)
  (buff-menu "buff-menu")
  (ztree ztree-diff)
  ))

(add-to-list 'load-path "~/.emacs.d/elpa/evil-collection/")
(require 'evil-collection)

(evil-collection-init)

; (load-file "~/.emacs.d/elpa/evil-collection/modes/ztree/evil-collection-ztree.el") (evil-collection-ztree-setup)


; (evil-define-key 'normal org-mode-map
;   (kbd "RET") 'org-open-at-point
;   "za"        'org-cycle
;   "zA"        'org-shifttab
;   "zm"        'hide-body
;   "zr"        'show-all
;   "zo"        'show-subtree
;   "zO"        'show-all
;   "zc"        'hide-subtree
;   "zC"        'hide-all
;   (kbd "M-j") 'org-shiftleft
;   (kbd "M-k") 'org-shiftright
;   (kbd "M-H") 'org-metaleft
;   (kbd "M-J") 'org-metadown
;   (kbd "M-K") 'org-metaup
;   (kbd "M-L") 'org-metaright)

; ; (add-to-list 'load-path "~/.emacs.d/evil/")

(message "evil grows in cracks and holes and lives in peoples' minds")

(define-key evil-normal-state-map "x" 'execute-extended-command) ;; N x = M-x
(define-key evil-normal-state-map [escape] nil) ; restore M- behavior in normal mode
