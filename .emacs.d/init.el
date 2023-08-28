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

(if (not (fboundp 'bind-key)) (progn
  (message "polyfilling bind-key")
  ;;(bind-key (kbd "C-<return>") #'my-org-ctrl-return-dwim 'org-mode-map)
  ;;(define-key global-map (kbd "<f5>") 'revert-buffer)
  (defmacro bind-key (key-name command &optional keymap)
    `(define-key (symbol-value (or ,keymap 'global-map) (kbd ,key-name) ,command)))
))

;;# Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 110 :width normal :spacing monospace))))
 '(comint-highlight-input ((t nil)))
 '(help-key-binding ((t (:background "#222" :foreground "#f6f3e8"))))
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
    :font (font-candidate
           "Consolas-12:weight=normal"
           "DejaVu Sans Mono-12:weight=normal"
           "Courier New-12:weight-normal")
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

(defun windowsify-path (path)
  (replace-regexp-in-string "/\\([a-z]\\)/" "\\1:/" path nil nil))

;;(defun regenerate-autoloads ()
;;  (interactive)
;;  ;(package-generate-autoloads package-name default-directory)
;;)

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

(defun string-to-macro (string)
  "Convert a string to a series of keystrokes appropriate for kbd-read-macro"
  (mapconcat 'identity ;;Map identity of list joined by space (last arg)
    (cl-substitute "SPC" " "   ;;Substitute SPC for actual spaces
      (split-string string "") ;;Split string by char
      :test #'string-equal     ;;Comparing spaces can be funky by default
    )
    " ") ;;The joiner
  )

(defun shell-term (new-buffer-name cmd &rest switches)
  "Create a new buffer running command `cmd' with the provided switches"
  ;;(setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name (or new-buffer-name "term")))
  (setq term-buffer (set-buffer (apply 'make-term term-ansi-buffer-name cmd nil switches)))
  (term-mode)
  (term-char-mode)
  term-buffer)

(defun termwith (title commandlist)
  "Create a new buffer running a bash shell and send it a list of lines"
  (let* ((shell-buff (shell-term title "/bin/bash"))
         (proc (get-buffer-process shell-buff)))
    (dolist (command commandlist)
      (accept-process-output proc)
      (term-send-string proc "\n")
      (accept-process-output proc)
      (term-send-string proc command))
    shell-buff))

(defun bash ()
  "Open bash in `ansi-term' with full login configuration"
  (interactive)
  (switch-to-buffer (shell-term nil "/bin/bash" "--login")))

(defun debug-on-error ()
  "Toggle `debug-on-error'"
  (interactive)
  (toggle "debug-on-error"))

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
(global-set-key (kbd "C-x f") ctl-x-5-map)

;;Focus buffer list instead of backgrounding
(bind-key "C-x C-b" #'buffer-menu-other-window)

;;Get back to minibuffer w/o mouse
(bind-key "C-c o" #'switch-to-minibuffer)

;;Mouse buttons
;;These don't work through Citrix. :(
(bind-key "<mouse-6>" #'next-buffer)
(bind-key "<mouse-7>" #'previous-buffer)

;;elisp eval functions
;; todo echo output to messages
(bind-key "C-c e" #'eval-buffer 'emacs-lisp-mode-map)
(bind-key "C-c r" #'eval-region 'emacs-lisp-mode-map)
(add-hook 'emacs-lisp-mode-hook (lambda ()
   (setq-local evil-shift-width 2)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq-local imenu-generic-expression
      (append
       '(("Headers" "^;; ?# ?\\(.+\\)" 1)
         ;;("h2" "^;; ?## ?\\(.+?\\)" 1)
         ;;("h" "^;; ?###+ ?\\(.+?\\)" 1)
         )
       imenu-generic-expression)
    )
  )
)

;;Unbind gnu cruft
(dolist
  (junk-key '(
    "C-h t"
    "C-h a"
    "C-h n"
    "C-h g"
    "C-h o"
    "C-h w"
  ))
  (global-unset-key (kbd junk-key)))

;;Refresh file
(bind-key "<f5>" #'revert-buffer)
(bind-key "<XF86Reload>" 'revert-buffer)

;;Quick reference keymap
(bind-key "C-h M" #'describe-keymap)

;;Use ctrl+shift+c/v in x11 mode, like a terminal.
(bind-key "C-S-C" #'kill-ring-save)
(bind-key "C-S-V" #'yank)

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
(bind-key "<M-right>" #'next-buffer)
(bind-key "<M-left>" #'previous-buffer)
(bind-key "<M-S-right>" #'other-window)
(bind-key "<M-S-left>" #'other-window-backwards)

;;function keys for some handy emacs things
(bind-key "<f12>" #'indent-buffer)
(bind-key "C-<f10>" #'menu-bar-open)
(bind-key "<f9>" #'sort-lines)
;; (bind-key "C-c r" #'replace-regexp)

;;ffap under user f
(bind-key "C-c C-f" #'ffap)

;;C-S-/ to toggle comment
(bind-key "C-?" #'comment-or-uncomment-region)

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
(bind-key "C-S-e" #'py-eval-region)

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

;; Space key inserts itself in minibuffer
(define-key minibuffer-local-completion-map " " 'self-insert-command)
(define-key minibuffer-local-must-match-map " " 'self-insert-command)

;;Use left alt as meta key
(setq x-alt-keysym 'meta)

(setq-default
  delete-by-moving-to-trash t)

;;Clean startup
(setq
  initial-scratch-message ""
  inhibit-startup-screen t
  inhibit-startup-message t)

;;Clean writing
(setq
  delete-trailing-lines nil        ; delete-trailing-whitespace doesn't delete lines
  next-line-add-newlines t         ; Add newlines automatically when past buffer end
  require-final-newline t
  x-stretch-cursor t
  truncate-string-elipsis t
  sentence-end-double-space nil)
(setq-default
  buffer-file-coding-system 'utf-8-unix ; Correct line endings
  indent-tabs-mode nil     ; Use spaces instead of tabs
  tab-width 4)              ; Use 4 spaces, not 8

;;Enhancements
(setq
  message-log-max 10000            ; Increase message log capacity
  undo-limit 10000                 ; Increase undo history capacity
  vc-follow-symlinks t             ; don't refuse to open version controlled symlinks
  diff-switches "-u"              ; default to unified diffs
  ring-bell-function 'ignore       ; neuter the bell
  visible-bell t
  query-replace-highlight t    ;highlight during query
  search-highlight t)           ;highlight incremental search

; (add-hook 'term-mode-hook
;   (lambda () (term-set-escape-char ?\C-x)))

(defun term-use-sensible-escape-char (&rest ignored)
  (term-set-escape-char 24))
(advice-add 'term :after #'term-use-sensible-escape-char)

(setq-default
  indicate-empty-lines t
  indicate-buffer-boundaries 'left
  frame-title-format (list "%b"))

(add-hook 'prog-mode-hook
  (lambda () (setq show-trailing-whitespace t))) ;Show stray whitespace.
;;Show info in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(make-directory "~/.tmp/emacs/backup/" t)
(setq
  auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t))
  backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

(setq
  savehist-file "~/.emacs.d/savehist"
  savehist-save-minibuffer-history +1
  savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode +1)

(setq
  backup-by-copying t ;Do not move the current file while creating backup.
  delete-by-moving-to-trash t
  create-lockfiles nil) ;Disable lockfiles.

;; Only show the menu bar in windowed mode
(if (not (window-system))
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Language settings

(setq
  sh-basic-offset 2
  python-shell-interpreter "python3")

(add-hook 'shell-mode-hook (lambda ()
   (setq-local evil-shift-width 2)))
(setq ess-fancy-comments nil)

(add-hook 'term-mode-hook
  (lambda ()
    (setq-local show-trailing-whitespace nil)
    (bind-key "C-S-v" #'term-paste 'term-raw-map)
    (bind-key "C-|" #'term-kill-subjob 'term-raw-map)
))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

(add-hook 'python-mode-hook
          #'(lambda () (outline-minor-mode 1)))

(add-hook 'python-mode-hook
  (lambda ()
    (setq-local imenu-generic-expression
      (append
       '(("Root Comment" "^# ?\\(.+\\)" 1)
         )
       imenu-generic-expression))))

(defun python-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
		       (* space)
		       (one-or-more (syntax comment-start))
		       (one-or-more space)
		       (group (one-or-more "\*"))
		       (one-or-more space))
		      (match-string 0))
	(- (match-end 0) (match-beginning 0)))

   ;; Python keyword heading, set by number of indentions
   ;; Add 8 (the highest standard outline level) to every Python keyword heading
   (+ 8 (- (match-end 0) (match-beginning 0)))))

(defun python-mode-outline-hook ()
  (setq outline-level 'python-outline-level)

  (setq outline-regexp
	(rx (or
	     ;; Commented outline heading
	     (group
	      (* space)	 ; 0 or more spaces
	      (one-or-more (syntax comment-start))
	      (one-or-more space)
	      ;; Heading level
	      ;; (group (repeat 1 8 "\*"))  ; Outline stars
	      ;; (one-or-more space))
          )

	     ;; Python keyword heading
	     (group
	      ;; Heading level
	      (group (* space))	; 0 or more spaces
	      bow
	      ;; Keywords
	      (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
	      eow)))))

(add-hook 'python-mode-hook 'python-mode-outline-hook)

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
(setq
 tramp-ssh-controlmaster-options
 (concat
   "-o ControlPath=~/.ssh/connections/%%r@%%h:%%p "
   "-o ControlMaster=auto -o ControlPersist=yes"))
(setq tramp-use-ssh-controlmaster-options nil)

(setq vc-handled-backends '(Git))
(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))

(setq ls-lisp-dirs-first t)             ; display dirs first in lisp-native dired
;; Enable fully editable wdired
(customize-set-variable 'wdired-allow-to-change-permissions t)
(eval-after-load "dired" '(progn
  (bind-key "C-}" #'dired-maybe-insert-subdir 'dired-mode-map)
  (bind-key "C-{" #'dired-hide-subdir 'dired-mode-map)
  (bind-key "<tab>" #'dired-hide-subdir 'dired-mode-map)
  (bind-key "C-k" #'dired-kill-subdir 'dired-mode-map)
))


;; (with-eval-after-load 'dired
;;   (require 'dired-x)
;;   ;; Set dired-x global variables here.  For example:
;;   ;; (setq dired-x-hands-off-my-keys nil)
;; ))

;;# Outlining

;; Sublime-style fold/unfold keys
(add-hook 'outline-minor-mode-hook #'(lambda ()
    (defun outline-overview ()
      "Show an overview of the body"
      (interactive)
      (outline-show-all)
      (outline-hide-body))
    (bind-key "C-!"     #'outline-toggle-children 'outline-minor-mode-map)
    (bind-key "C-<S-1>" #'outline-toggle-children 'outline-minor-mode-map)
    (bind-key "C-{"     #'outline-hide-subtree 'outline-minor-mode-map)
    (bind-key "C-<S-[>" #'outline-hide-subtree 'outline-minor-mode-map)
    (bind-key "C-}"     #'outline-show-subtree 'outline-minor-mode-map)
    (bind-key "C-<S-]>" #'outline-show-subtree 'outline-minor-mode-map)
    (bind-key "<C-return>" #'outline-cycle 'outline-minor-mode-map)
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
  (lambda () (bind-key "B" #'ediff-copy-both-to-C 'ediff-mode-map)))

(setq ediff-window-setup-function 'ediff-setup-windows-default) ;Frame configuration
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)

;; Run/highlight code using babel in org-mode
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (shell . tool-bar-mode) ; (sh . tool-bar-mode) in older emacs versions
     ;; Include other languages here...
     ))
  (defun my-org-ctrl-return-dwim ()
    "DWIM ctrl-return in org mode: execute babel block or insert heading"
    (interactive)
    (if (org-babel-get-src-block-info 'light)
        (org-babel-execute-maybe)
      (org-insert-heading-respect-content))
  )
  (bind-key "C-<return>" #'my-org-ctrl-return-dwim 'org-mode-map)

  ;; :hidden property on source blocks folds them when file opened
  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer."
    (interactive)
      ;; (org-show-block-all)
      (org-block-map (lambda ()
        (let ((case-fold-search t))
          (when (and
                 (save-excursion
                   (beginning-of-line 1)
                   (looking-at org-block-regexp))
                 (cl-assoc
                  ':hidden
                  (cl-third
                   (org-babel-get-src-block-info t))))
            (org-hide-block-toggle t))))))
  (add-hook
   'org-mode-hook
   #'individual-visibility-source-blocks)
)
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;;===================================================
;;# Bootstrap: Now load another file

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'inferior-term)

(dolist (exfile '(
  "~/.emacs.d/init-extra.el"
  "~/.emacs.d/init-local.el"
  "~/.emacs.d/ev-init.el"))
    (if (file-exists-p exfile) (load-file exfile)))

;;=====================================================================
