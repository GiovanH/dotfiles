(add-to-list 'load-path "~/.emacs.d/evil/")

(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-overriding-maps nil)

(require 'evil)

(message "evil grows in the dark")

(require 'evil-collection)
(evil-collection-init)

(evil-mode 1)

(define-key evil-normal-state-map "x" 'execute-extended-command) ;; N x = M-x
(define-key evil-normal-state-map [escape] nil) ; restore M- behavior in normal mode

;not working
;(define-key evil-visual-state-map (kbd "<tab>") indent-region)

;ugly
; (setq evil-normal-state-tag (propertize "N" 'face '())
;       evil-emacs-state-tag  (propertize "E" 'face '((:background "#93a1a1")))
;       evil-insert-state-tag (propertize "I" 'face '((:background "#93a1a1")))
;       evil-motion-state-tag (propertize "M" 'face '())
;       evil-visual-state-tag (propertize "V" 'face '())
;       evil-operator-state-tag (propertize "O" 'face '())
;       evil-cross-lines t
;       evil-want-C-w-in-emacs-state t)

;(setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
;      evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
;      evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
;      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
;      evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
;      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

;; evil-collection replaces

; (evil-set-initial-state 'dired-mode 'emacs)

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

;; Broken
; (setq evil-default-cursor #'cofi/evil-cursor)
; (defun cofi/evil-cursor ()
;   "Change cursor color according to evil-state."
;   (let ((default "OliveDrab4")
;         (cursor-colors '((insert . "dark orange")
;                          (emacs  . "sienna")
;                          (visual . "white"))))
;     (setq cursor-type (if (eq evil-state 'visual)
;                           'hollow
;                         'bar))
;     (set-cursor-color (def-assoc evil-state cursor-colors default))))

(setq evil-undo-system 'undo-fu)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
