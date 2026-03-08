(defun my-key-quiz ()
  "Personal key quiz."
  (interactive)
  (require 'key-quiz)

  (setq key-quiz-game-length -1)

  (defun key-quiz--ask ()
    "Prompt the player for a key corresponding to a command.
  A random element from `key-quiz--keys' is chosen, and the player is
  shown the chosen command.  The player must then guess one of the keys
  corresponding to the command (as there may be more than one).
  Finally, return (SCORE . CORRECT-ANSWER), where SCORE is a number
  \(positive or negative) which should be added to `key-quiz--score',
  and CORRECT-ANSWER is the correct answer in case the player did not
  answer correctly, or nil otherwise."
    (let* ((pair (if key-quiz--last-state
            (car key-quiz--last-state)
          (seq-random-elt key-quiz--keys)))
      (command (nth 1 pair))
      (keymap-name (nth 2 pair))
      (keys (if key-quiz--last-state
            (cdr key-quiz--last-state)
          ;; One command may be bound to multiple keys, fetch them all.
          (mapcar 'car (seq-filter (lambda (p)
                          (string= (nth 1 p) command))
                        key-quiz--keys))))
      (all-keys (mapconcat 'identity keys " or "))
      (best-matches nil)
      result
      entered-key)
      (if key-quiz--last-state
      ;; We are resuming, do not modify key list again.
      (setq key-quiz--last-state nil)
        ;; First time using this key-command pair.
        (dolist (key keys)
      ;; Delete all key-command pairs that map to the same command
      ;; from global key-command list. This way we don't ask for the
      ;; same command more than once.
      (setf key-quiz--keys (cl-delete key key-quiz--keys
                      :key #'car :test #'equal))))
      (setq key-quiz--prompt-pos (point))
      (insert (format "For keymap %s, " keymap-name))
      (insert "Enter key for command:")
      (newline 2)
      (insert "    " (propertize command 'font-lock-face 'key-quiz-question))
      (let ((description (key-quiz--command-description command)))
        (when description
      (newline)
      (insert "    " (propertize description 'font-lock-face 'italic))))
      (newline 2)
      (when (> (length keys) 1)
        (insert (format "There are %s possible answers." (length keys)))
        (newline))
      (insert "Your answer: ")
      (setq entered-key (key-description (read-key-sequence-vector
                      "Key (RET to give up): ")))
      (cond
      ((string= entered-key "C-g") (throw 'end t))
      ((string= entered-key "p")
        ;; Save game state for resume.
        (setq key-quiz--last-state (cons pair keys))
        (throw 'end 'pause))
      ((string= entered-key "RET") (setq entered-key "")))
      (insert entered-key)
      (dolist (key keys)
        (let* ((matches-total (key-quiz--keys-distance entered-key key))
          (matches (car matches-total))
          (is-total (cdr matches-total)))
      (if (or (not best-matches)
            (< best-matches matches)
            is-total)
        (progn
          (setq result
            (cons (* matches key-quiz-partial-answer-score)
              (unless is-total all-keys))
            best-matches matches)
          ))))
      (insert (format "\nCorrect answer was: %s\n"
                (propertize all-keys)
                'font-lock-face 'key-quiz-answer))
      (if (cdr result)
        (setq key-quiz--last-state (cons pair keys)
          ))
      result))

  (defun keypairs-from-functions (fns keymap)
    (defun flatten (list-of-lists)
      (apply #'append list-of-lists))

    (flatten (mapcar
      (lambda (fn)
        (let ((pairs
          (mapcar
            ;; Construct keypair
            (lambda (key) (list
                            key
                            (symbol-name fn)
                            keymap))
            ;; Get seq of key options
            (mapcar #'key-description (where-is-internal fn (symbol-value keymap)))
            )
          ))
          (if (>= (length pairs) 1)
            pairs
            ;; else
            ;; `((concat "M-x " ,(symbol-name fn)) . ,(symbol-name fn))
            (error "Could not resolve all: %s %s %s " fn pairs (length pairs))

            )
          ))
      fns)))

  (key-quiz nil
    ;; Construct (key . command) list
    (append
      (keypairs-from-functions
        '(
           back-to-indentation
           comment-or-uncomment-region
           buffer-menu-other-window
           capitalize-word
           dabbrev-expand
           describe-keymap
           downcase-region
           downcase-word
           eval-expression
           ffap
           ;; find-file
           goto-char
           find-name-dired
           ;; goto-line ;; replaced by evil-ex
           ;; indent-buffer
           indent-rigidly
           kmacro-end-or-call-macro
           kmacro-start-macro-or-insert-counter
           mark-defun
           menu-bar-open
           next-buffer
           other-window
           other-window-backwards
           previous-buffer
           recenter-top-bottom
           ;; save-buffer
           save-some-buffers
           shell-command-on-region
           sort-lines
           switch-to-minibuffer
           transpose-lines
           upcase-region
           upcase-word
           find-name-dired
         ) 'global-map)
      ;; User
      (keypairs-from-functions
        '(
           swiper
           ace-window
           ;; ivy-mode
           avy-goto-char
           counsel-imenu
           crux-eval-and-replace
           crux-rename-file-and-buffer
           py-eval-region
           ;; efar
           crux-eval-and-replace
           crux-rename-file-and-buffer
           crux-transpose-windows
           ;; sr-speedbar-open
           ;; ztree
           ;; ztree-dir
           undo-fu-only-undo
           hydra-flycheck/body
           undo-fu-only-redo
           evil-quick-replace-selection
        ) 'global-map)
      (keypairs-from-functions
        '(
           evil-forward-paragraph
           evil-backward-paragraph
           evil-scroll-line-down
           evil-scroll-line-up
           evil-goto-mark-line
           evil-window-middle
           evil-window-bottom
           evil-forward-WORD-begin
        ) 'evil-motion-state-map)
      (keypairs-from-functions
        '(
           ;; evil-ex
           evil-open-fold
           evil-close-fold
           evil-open-above
           evil-open-below
           ;;evil-replace-state
           evil-join
           execute-extended-command
           quit-window
           revert-buffer
           evil-set-marker
           ;; eval-buffer
           ;; eval-print-last-sexp
           ;; evil-execute-in-emacs-state
        ) 'evil-normal-state-map)
      (keypairs-from-functions
        '(
           evil-quoted-insert
           evil-paste-last-insertion
           evil-shift-left-line
           evil-shift-right-line
           evil-insert-digraph
        ) 'evil-insert-state-map)
      (keypairs-from-functions
        '(
           dired-hide-details-mode
           ;; dired-maybe-insert-subdir
        ) 'dired-mode-map)
    )
  )
)
