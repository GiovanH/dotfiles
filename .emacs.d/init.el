;;=====================================================================
;; gio's really great emacs configuration file

;; This is the default config file, run automatically every startup.
;; This includes general configuration and settings, and can be adjusted to taste.

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
))

;;# Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 110 :width normal :spacing monospace))))
 '(linkd-generic-link ((t (:inherit bookmark-menu-heading))))
 '(region ((t (:background "#000" :foreground "#f6f3e8"))))
 '(trailing-whitespace ((t (:background "unspecified" :underline "#CC9393"))))
 '(ztreep-diff-model-add-face ((t (:inherit diff-refine-added))))
 '(ztreep-diff-model-diff-face ((t (:inherit diff-refine-removed)))))

(defun font-candidate (&rest fonts)
  "Return existing font which first matches"
  (require 'cl)
  (cl-find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(ignore-errors
  (set-face-attribute 'default nil
    :font (font-candidate "Consolas-12:weight=normal" "DejaVu Sans Mono-12:weight=normal")
    :height 110)
  ;; (when (member "Noto Emoji" (font-family-list))
  ;;   (set-fontset-font
  ;;    t 'symbol (font-spec :family "Noto Emoji") nil 'prepend)
  ;; )
  ;; Test: ????
)

(progn
  ;; set font for emoji (if before emacs 28, should come after setting symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
     '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")

;; Use a decent color theme
(load-theme 'wombat)
;; Otherwise, you probably shouldn't use colors at all. (Uncomment this:)
;;(global-font-lock-mode 0)

;;=====================================================================
;;# Commands

;; (defun command-line-diff (switch)
;;   (let ((file1 (pop command-line-args-left))
;;         (file2 (pop command-line-args-left)))
;;     (ediff file1 file2)))

;; (add-to-list 'command-switch-alist '("diff" . command-line-diff))

(defun toggle (opt)
  "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates."
  (interactive
   (list (completing-read
	  "Toggle value of option: " obarray
	  (cond ((and current-prefix-arg
		      (wholenump (prefix-numeric-value current-prefix-arg)))
		 'user-variable-p)
		(current-prefix-arg 'boundp)
		(t (lambda (sym) (eq (get sym 'custom-type) 'boolean))))
	  t nil 'variable-name-history)))
  (let ((sym (intern opt)))
    (set sym (not (eval sym))) (message "`%s' is now %s" opt (eval sym))))

(defun org-toggle-wysiwyg ()
  (interactive)
  (toggle "org-hide-emphasis-markers")
  ; (org-bullets-mode org-hide-emphasis-markers)
)

(defun indent-buffer ()
  "Automatically indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun bash ()
  "Open bash in `ansi-term'"
  (interactive)
  (ansi-term "/bin/bash"))

(defun debug-on-error ()
  "Set `debug-on-error' to t"
  (interactive)
  (setq debug-on-error t))

(defun copy-current-kill-to-clipboard ()
  "Backup function for copying to system clipboard (unused)"
  (interactive)
  (x-select-text (current-kill 0)))

(defun my-byte-recompile ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun my-reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun cycle-tab-width ()
  "Cycle `tab-width' between 2 and 4"
  (interactive)
  (if (eq tab-width 4)
      (setq tab-width 2)
    (setq tab-width 4))
  (message "Tab width %s" tab-width))

(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
    (list
    (completing-read
    "Zone out to: "
    (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
    (zone)))

(defun theme-mark-alt-buffer ()
  "Visually mark the current buffer with an alternative scheme"
  (interactive)
  (face-remap-add-relative 'default
                           :background "black"
                           ;;:foreground "lightblue"))
                           ))

;;=====================================================================
;;# Keys

;;Quick access to frame methods w/ C-x f
(global-set-key "\C-xf" ctl-x-5-map)

;;Focus buffer list instead of backgrounding
(global-set-key "\C-x\C-b" 'buffer-menu-other-window)

;;Mouse buttons
;;These don't work through Citrix. :(
(global-set-key (kbd "<mouse-6>") 'next-buffer)
(global-set-key (kbd "<mouse-7>") 'previous-buffer)

;;elisp eval functions
(add-hook 'emacs-lisp-mode-hook
  (lambda () (local-set-key (kbd "C-c e") 'eval-buffer)))
(add-hook 'emacs-lisp-mode-hook
  (lambda () (local-set-key (kbd "C-c r") 'eval-region)))


(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq-local imenu-generic-expression
      (append
       '(("Headers" "^;; ?# ?\\(.+\\)" 1)
         ;("h2" "^;; ?## ?\\(.+?\\)" 1)
                                        ;("h" "^;; ?###+ ?\\(.+?\\)" 1)
         )
       imenu-generic-expression)
    )
  )
)


;;Refresh file
(define-key global-map (kbd "<f5>") 'revert-buffer)
(define-key global-map (kbd "<XF86Reload>") 'revert-buffer)

;; not working :'(
;; (defun grep-recursive-directory ()
;;   (interactive); "D")
;;   (grep-apply-setting 'grep-template "grep <X> <C> <R> -r .")
;;   (call-interactively 'grep))
;; (define-key dired-mode-map (kbd "C-c g") 'grep-recursive-directory)

;;Use ctrl+shift+c/v in x11 mode, like a terminal.
(global-set-key (kbd "C-S-C") 'kill-ring-save)
(global-set-key (kbd "C-S-V") 'yank)

;;Backwards window nav for 3+ windows.
(defun other-window-backwards ()
  (interactive)
  (other-window -1))
;;In terminal mode, arrows are bound to M-O {a,b,c,d}, so M-O is a prefix.
;;(global-set-key (kbd "M-o n") 'other-window)
;;(global-set-key (kbd "M-o p") 'other-window-backwards)
;;this is kinda funky but it mirrors default C-x o
;;(global-set-key (kbd "C-x O") 'other-window-backwards)

;;Cycle buffers with web page forward/backward keys
(global-set-key (kbd "<M-right>") 'next-buffer)
(global-set-key (kbd "<M-left>") 'previous-buffer)
(global-set-key (kbd "<M-S-right>") 'other-window)
(global-set-key (kbd "<M-S-left>") 'other-window-backwards)

;;function keys for some handy emacs things
(global-set-key [f12] 'indent-buffer)
(global-set-key (kbd "C-<f10>") 'menu-bar-open)
(global-set-key (kbd "<f9>") 'sort-lines)
(global-set-key (kbd "C-c r") 'replace-regexp)

;;ffap under user f
(global-set-key (kbd "C-c C-f") 'ffap)

;;C-S-/ to toggle comment
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)

(defun apply-to-region (func)
  (undo-boundary)
  (let* ((substr (buffer-substring (mark) (point)))
        (res (funcall func substr)))
    (delete-region (region-beginning) (region-end))
    (insert res)))

(defun replace-or-display (arg expr result)
  "Replace the region with `result, or just display that `expr = `result if arg is not null."
  (if (null arg)
      (apply-to-region (lambda (r) result))
    (message "%s = %s" expr result)
))

(defun calc-eval-region (arg start end)
  "Replace the region with the result of evalulating it in `calc, or just display it with a prefix argument."
  (interactive "P\nr")
  (let ((expr (buffer-substring-no-properties start end)))
    (replace-or-display arg expr (calc-eval expr)))
)

(defun py-eval-region (arg start end)
  "Replace the region with the result of evalulating it in `python3, or just display it with a prefix argument."
  (interactive "P\nr") ; unused
  (let* ((tmpfile (make-temp-file "py-eval-inline" nil ".py"))
         (expr (buffer-substring-no-properties start end))
         (__ (write-region (format "import sys\nimport math\nsys.stdout.write(str(eval('''%s''')))" expr) nil tmpfile))
         (res (shell-command-to-string (format "python3 %s" tmpfile))))
      (delete-file tmpfile)
      (replace-or-display arg expr res))
  )
(global-set-key (kbd "C-S-e") 'py-eval-region)

;;=====================================================================
;;#Hooks

(defvar *afilename-cmd*
  '(((expand-file-name "~/.Xresources") . "xrdb -merge ~/.Xresources")
    ((expand-file-name "~/.xbindkeysrc") . "xbindkeys -p")
    ((expand-file-name "~/.ssh/config.j2") . "j2 ~/.ssh/config.j2"))
  "File association list with their respective command.")

(defun auto-cmd-after-saved-file ()
  "Execute a command after saved a specific file."
  (let* ((match (assoc (buffer-file-name) *afilename-cmd*)))
    (when match
      (shell-command (cdr match)))))

(add-hook 'after-save-hook 'auto-cmd-after-saved-file)


(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

;;Automatically clean up files
(add-hook 'before-save-hook
  (lambda ()
    (my-delete-trailing-blank-lines)
    ;markdown files use trailing whitespace for newlines
    (unless (eq major-mode 'markdown-mode)
      (delete-trailing-whitespace))))

;;When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;Making C-x k end an emacsclient session
(add-hook 'server-switch-hook
      (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))

;;Restore frames when loading desktop file
(setq desktop-restore-forces-onscreen nil)
(if (fboundp 'frameset-restore)
  (add-hook 'desktop-after-read-hook
    (lambda ()
      (frameset-restore
        desktop-saved-frameset
        :reuse-frames (eq desktop-restore-reuses-frames t)
        :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
        :force-display desktop-restore-in-current-display
        :force-onscreen desktop-restore-forces-onscreen))))

;;=====================================================================
;;#Configuration

;;Use left alt as meta key
(setq x-alt-keysym 'meta)

;;Clean startup
(setq initial-scratch-message "")
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;;Clean writing
(setq delete-trailing-lines nil)        ; delete-trailing-whitespace doesn't delete lines
(setq-default indent-tabs-mode nil)     ; Use spaces instead of tabs
(setq-default tab-width 4)              ; Use 4 spaces, not 8
(setq next-line-add-newlines t)         ; Add newlines automatically when past buffer end
(setq-default buffer-file-coding-system 'utf-8-unix) ; Correct line endings
(setq sentence-end-double-space nil)
(setq require-final-newline t)

;;Enhancements
(setq message-log-max 10000)            ; Increase message log capacity
(setq undo-limit 10000)                 ; Increase undo history capacity
(setq vc-follow-symlinks t)             ; don't refuse to open version controlled symlinks
(setq diff-switches "-u")               ; default to unified diffs
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default frame-title-format (list "%b @Emacs"))
(setq ring-bell-function 'ignore)       ; neuter the bell
(setq visible-bell t)
(add-hook 'prog-mode-hook
  (lambda () (setq show-trailing-whitespace t))) ;Show stray whitespace.
(setq query-replace-highlight t)    ;highlight during query
(setq search-highlight t)           ;highlight incremental search
;;Show info in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(make-directory "~/.tmp/emacs/backup/" t)
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

;; Only show the menu bar in windowed mode
(if (not (window-system))
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Language settings

(setq sh-basic-offset 2)
(setq ess-fancy-comments nil)

(add-hook 'term-mode-hook
  (lambda ()
    (setq-local show-trailing-whitespace nil)
    (define-key term-raw-map (kbd "C-S-v") 'term-paste)
))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

(add-to-list 'auto-mode-alist '("build.gradle" . java-mode))

;; Look in cwd for a desktop file (project-style)
(setq desktop-path '("~/.emacs.d/" "~" "."))

;;=====================================================================
;;#Packages

(show-paren-mode 1)  ; Highlight parenthesis pairs
(setq show-paren-delay 0)
(setq speedbar-use-images nil)
(winner-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Environment workaround
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list (expand-file-name "~/.emacs.d/magit-3.3.0/"))
  (add-to-list 'Info-directory-list (expand-file-name "~/.local/info/"))
  (add-to-list 'Info-directory-list (expand-file-name "~/.local/share/info/"))
  (add-to-list 'Info-directory-list (expand-file-name "~/info/")))

;;Tramp
(require 'tramp)
(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=~/.ssh/connections/%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes"))
(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)

(setq vc-handled-backends '(Git))
(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))

(setq ls-lisp-dirs-first t)             ; display dirs first in lisp-native dired
;; Enable fully editable wdired
(customize-set-variable 'wdired-allow-to-change-permissions t)

;;Outlining

;; Sublime-style fold/unfold keys
(add-hook 'outline-minor-mode-hook #'(lambda ()
    (defun outline-overview ()
      "Show an overview of the body"
      (interactive)
      (outline-show-all)
      (outline-hide-body))
    (local-set-key (kbd "C-!")     'outline-toggle-children)
    (local-set-key (kbd "C-<S-1>") 'outline-toggle-children)
    (local-set-key (kbd "C-{")     'outline-hide-subtree)
    (local-set-key (kbd "C-<S-[>") 'outline-hide-subtree)
    (local-set-key (kbd "C-}")     'outline-show-subtree)
    (local-set-key (kbd "C-<S-]>") 'outline-show-subtree)
    (local-set-key (kbd "<C-return>") 'outline-cycle)
))

;; (add-hook 'prog-mode-hook       'outline-minor-mode)

(defun ediff-copy-both-to-C ()
  "In `ediff', copy the text from both A and B to C, for hand-editing."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(add-hook 'ediff-keymap-setup-hook
  (lambda () (define-key ediff-mode-map "B" 'ediff-copy-both-to-C)))

(setq ediff-window-setup-function 'ediff-setup-windows-default) ;Frame configuration
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)

;; Run/highlight code using babel in org-mode
(eval-after-load 'org'
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (shell . tool-bar-mode) ; (sh . tool-bar-mode) in older emacs versions
     ;; Include other languages here...
     )))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;;===================================================
;;# Bootstrap: Now load another file

(dolist (exfile '("~/.emacs.d/init-extra.el" "~/.emacs.d/init-local.el"))
  (if (file-exists-p exfile) (load-file exfile)))

;;=====================================================================
