(eval-when-compile (require 'cl-lib))
(require 'gamegrid)

;; Customization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup floodit nil
  "Play a game of Floodit."
  :prefix "floodit-"
  :group 'games)

(defvar floodit-score-file
  "~/.tmp/floodit-scores"
  "File for holding high scores.")

(defvar floodit-mode-map
  (let ((map (make-sparse-keymap 'floodit-mode-map)))
    (define-key map (kbd "<mouse-1>") 'floodit-click)
    (define-key map (kbd "SPC") 'floodit-click)
    (define-key map (kbd "<mouse-2>") 'floodit-click)
    map))

(defcustom floodit-symbol-list
  '(?a ?s ?d ?f ?j ?k ?l)
  "List of the possible values"
  :group 'floodit
  :type 'sexp)

(defcustom floodit-use-glyphs nil
  "Non-nil means use glyphs when available."
  :group 'floodit
  :type 'boolean)

(defcustom floodit-use-color t
  "Non-nil means use color when available."
  :group 'floodit
  :type 'boolean)

(defcustom floodit-mode-hook nil
  "Hook run upon starting Flood-it."
  :group 'floodit
  :type 'hook)

(defcustom floodit-tty-colors
  '((?a . "red")
    (?s . "green")
    (?d . "blue")
    (?f . "magenta")
    (?j . "cyan")
    (?k . "yellow")
    (?l . "white")
    (?h . "grey")
    )
  "Alist of colors of the various symbols in text mode"
  :group 'floodit
  :type 'sexp)

(defcustom floodit-x-colors
  '((?a . [.80 .00 .00]) ;[.8 .5 .5])
    (?s . [.45 .82 .09]) ;[.5 .8 .5])
    (?d . [.20 .40 .80]) ;[.5 .5 .8])
    (?f . [.62 .31 .62]) ;[.8 .5 .8])
    (?j . [.45 .62 .81]) ;[.5 .8 .8])
    (?k . [.93 .83 .00]) ;[.8 .8 .5])
    (?l . [.90 .90 .80]) ;[.8 .8 .8])
    (?h . [.00 .00 .00])) ;[.5 .5 .5]))
  "Alist of colors of the various symbols in X mode"
  :group 'floodit
  :type 'sexp)

(defcustom floodit-buffer-name "*Flood-it*"
  "Name used for Floodit buffer."
  :group 'floodit
  :type 'string)

(defcustom floodit-buffer-width 22
  "Width of used portion of buffer."
  :group 'floodit
  :type 'number)

(defcustom floodit-buffer-height 20
  "Height of used portion of buffer."
  :group 'floodit
  :type 'number)

(defcustom floodit-board-width 12
  "Width of playing area."
  :group 'floodit
  :type 'number)

(defcustom floodit-board-height 12
  "Height of playing area."
  :group 'floodit
  :type 'number)

(defcustom floodit-top-left-x 1
  "X position of top left of playing area."
  :group 'floodit
  :type 'number)

(defcustom floodit-top-left-y 1
  "Y position of top left of playing area."
  :group 'floodit
  :type 'number)

;; Cells

(defconst floodit-cell-blank 0)

;; Display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun floodit-char-options (c)
  ; Each spec in the vector maps c -> '(glyph-spec-list face-spec-list color-spec-list)
  ; For each char c,
  ; gamegrid-face-table[c]    = (face-spec-list color-spec-list)
  ; gamegrid-display-table[c] = (glyph-spec-list color-spec-list)
  ; Spec lists are alists associating names with values?
  ; i.e.
  ; ; For char c,
  ; (
  ;  ; Glyph spec list
  ;  ((glyph colorize) (emacs-tty ?O) (t 32))
  ;  ; Face spec list
  ;  ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
  ;  ; Color spec list
  ;  (((glyph color-x) [1 1 1]) (color-tty "white")))
  ; )
  (let ((symbol (cond
                 ((and floodit-use-glyphs floodit-use-color) ?#); █)
                 (t (capitalize c))
       )))
  `(((glyph colorize)
     (color-tty ,symbol)
     (t ,symbol))

    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty)
     (t nil))

    (((glyph color-x) ,(cdr (assoc c floodit-x-colors)))
     (color-tty ,(cdr (assoc c floodit-tty-colors)))
     (t nil))
  )
))

(defvar floodit-space-options
  '(((t 32))     nil     nil))

(defun floodit-display-options ()
  ; A 256-long vector that spans the ascii charset
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
        ; option[c] =
        (cond
          ((memq c floodit-symbol-list)
           (floodit-char-options c))
          ((= c floodit-cell-blank) floodit-space-options)
          (t '(nil nil nil))
      )))
    options))

;;

;; Drawing

(defun floodit-draw-score ()
  (let ((string (format "MOVES %02d" floodit-score)))
    (dotimes (x (length string))
      (gamegrid-set-cell x (+ floodit-board-height floodit-top-left-y 1) (aref string x)))
  )
)

;; Game ;

(defvar floodit-score 0)
(make-variable-buffer-local 'floodit-score)

(defun floodit-debug-gg ()
  (dotimes (i (length floodit-symbol-list))
    (if floodit-use-glyphs (gamegrid-set-cell (+ floodit-board-width 1 floodit-top-left-x) (+ 1 i) (capitalize (nth i floodit-symbol-list))))
    (gamegrid-set-cell (+ floodit-board-width 2 floodit-top-left-x) (+ 1 i) (nth i floodit-symbol-list)))
)

(defun floodit-set-cell (x y c)
  (gamegrid-set-cell
   (+ floodit-top-left-x x)
   (+ floodit-top-left-y y)
   c))

(defun floodit-get-cell (x y)
  (gamegrid-get-cell
   (+ floodit-top-left-x x)
   (+ floodit-top-left-y y)))

(defun floodit-init-buffer ()
  (gamegrid-init-buffer
    floodit-buffer-width
    floodit-buffer-height
    floodit-cell-blank)
  (dotimes (y floodit-board-height)
    (dotimes (x floodit-board-width)
      (floodit-set-cell x y
       (nth (random (length floodit-symbol-list)) floodit-symbol-list))
      ))
  (floodit-debug-gg))

(defun floodit-reset-game ()
  (gamegrid-kill-timer)
  (floodit-init-buffer))

(defun floodit-start-game ()
  (interactive nil floodit-mode)
  (floodit-reset-game)
  (use-local-map floodit-mode-map)
  (dolist (c floodit-symbol-list)
    (local-set-key (kbd (string c))
      `(lambda () (interactive) (floodit-do-move ,c))))
  (setq floodit-score 0)
  (floodit-draw-score)
)

(defun floodit-do-move (c)
  (let ((i (floodit-get-cell 0 0)) (new-filled-cells 0) (filled-cells '()) (already-filled-cells '()))
    (defun flood-fill (x y)
      (let ((v (floodit-get-cell x y)))
        (if (and
              (>= x 0) (< x floodit-board-width)
              (>= y 0) (< y floodit-board-height)
              (or (= v i)) ; (= v c)) ; if cell was the color of 0,0 or the target
              (not (or ; not already filled
                    (member (list x y) filled-cells) ; by us
                    (member (list x y) already-filled-cells))) ; already
            )
          (progn
            (push (list x y) filled-cells)
            (incf new-filled-cells)
            (floodit-set-cell x y c)
            (flood-fill (+ x 1) y)
            (flood-fill (- x 1) y)
            (flood-fill x (+ y 1))
            (flood-fill x (- y 1))
          )
          ; else
          (if (and
               (not (member (list x y) filled-cells))
               (= v c))
              (push (list x y) filled-cells))
        )
      )
    )
    (flood-fill 0 0)
    (setq filled-cells (length filled-cells))
    (if (not (or
              (= new-filled-cells 0)
              (= new-filled-cells filled-cells)))
        (setq floodit-score (incf floodit-score)))
    (floodit-draw-score)
    (let ((total-cells (- (* floodit-board-width floodit-board-height) 0 )))
      (message (format "Filled %s (%s/%s)" new-filled-cells filled-cells total-cells))
      (if (not (= filled-cells total-cells)) (ignore)
        (gamegrid-add-score floodit-score-file floodit-score) ; t) to reverse in modern emacs
        (message "Won in %s moves" floodit-score)
        (floodit-start-game))
    )
  )
)

(defun floodit-click ()
  (interactive)
  (let ((char (char-after (point)))
        ;(inhibit-read-only t)
        ;(row (bubbles--row (point)))
        ;(col (bubbles--col (point)))
        )
    (if (member char floodit-symbol-list) (floodit-do-move char)))
)

;; Admin

(put 'floodit-mode 'mode-class 'special)

(define-derived-mode floodit-mode special-mode "Floodit"
  :interactive nil

  (defun gamegrid-setup-face (face color)
    (set-face-foreground face color)
    ;(set-face-bold-p face t)
    ;(set-face-background face color)
    (gamegrid-set-font face)
    (set-face-background-pixmap face nil))

  (setq max-lisp-eval-depth (* 8 floodit-board-width floodit-board-height))
  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)
  ;(use-local-map floodit-null-map)
  (setq gamegrid-use-glyphs floodit-use-glyphs)
  (setq gamegrid-use-color floodit-use-color)
  (gamegrid-init (floodit-display-options)))

;;;###autoload
(defun floodit ()
  (interactive)

  (switch-to-buffer floodit-buffer-name)
  (gamegrid-kill-timer)
  (floodit-mode)
  (floodit-start-game))


;; end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'floodit)
