;;; tetris.el --- implementation of Tetris for Emacs

;; Copyright (C) 1997, 2001-2014 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 2.01
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tetris nil
  "Play a game of Tetris."
  :prefix "tetris-"
  :group 'games)

(defcustom tetris-use-glyphs nil
  "Non-nil means use glyphs when available."
  :group 'tetris
  :type 'boolean)

(defcustom tetris-use-color nil
  "Non-nil means use color when available."
  :group 'tetris
  :type 'boolean)

(defcustom tetris-draw-border-with-glyphs t
  "Non-nil means draw a border even when using glyphs."
  :group 'tetris
  :type 'boolean)

(defcustom tetris-default-tick-period 0.3
  "The default time taken for a shape to drop one row."
  :group 'tetris
  :type 'number)

(defcustom tetris-update-speed-function
  'tetris-default-update-speed-function
  "Function run whenever the Tetris score changes.
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped.
ROWS is the number of rows which have been completed.

If the return value is a number, it is used as the timer period."
  :group 'tetris
  :type 'function)

(defcustom tetris-mode-hook nil
  "Hook run upon starting Tetris."
  :group 'tetris
  :type 'hook)

(defcustom tetris-tty-colors
  ["yellow" "orange" "blue" "green" "red" "magenta" "cyan"]
  "Vector of colors of the various shapes in text mode."
  :group 'tetris
  :type '(vector (color :tag "Shape O")
		 (color :tag "Shape L")
		 (color :tag "Shape J")
		 (color :tag "Shape S")
		 (color :tag "Shape Z")
		 (color :tag "Shape T")
		 (color :tag "Shape I")))

(defcustom tetris-x-colors
  [[1 1 0] [1 .5 0] [0 0 1] [0 1 0] [1 0 0] [1 0 1] [0 1 1]]
  "Vector of colors of the various shapes."
  :group 'tetris
  :type 'sexp)

(defcustom tetris-buffer-name "*Tetris*"
  "Name used for Tetris buffer."
  :group 'tetris
  :type 'string)

(defcustom tetris-buffer-width 30
  "Width of used portion of buffer."
  :group 'tetris
  :type 'number)

(defcustom tetris-buffer-height 22
  "Height of used portion of buffer."
  :group 'tetris
  :type 'number)

(defcustom tetris-width 10
  "Width of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-height 20
  "Height of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-top-left-x 3
  "X position of top left of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-top-left-y 1
  "Y position of top left of playing area."
  :group 'tetris
  :type 'number)

(defcustom tetris-wall-kick-table '([0 0] [0 1] [0 -1] [1 0] [-1 0])
  "List of offsets to use when attempting to resolve a rotation"
  :group 'tetris
  :type 'sexp)

(defvar tetris-next-x (+ (* 2 tetris-top-left-x) tetris-width)
  "X position of next shape.")

(defvar tetris-next-y tetris-top-left-y
  "Y position of next shape.")

(defvar tetris-hold-x (+ (* 2 tetris-top-left-x) tetris-width)
  "X position of hold shape.")

(defvar tetris-hold-y (+ 5 tetris-next-y)
  "Y position of hold shape.")

(defvar tetris-score-x tetris-next-x
  "X position of score.")

(defvar tetris-score-y (+ tetris-hold-y 6)
  "Y position of score.")

(defcustom tetris-lockdown-end 3
  "Number of game ticks before pieces lock down"
  :group 'tetris
  :type 'number)

;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp
;; pointing to a file you don't want to clobber.
(defvar tetris-score-file "tetris-scores"
;; anybody with a well-connected server want to host this?
;(defvar tetris-score-file "/anonymous@ftp.pgt.com:/pub/cgw/tetris-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar tetris-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    ;; color information is taken from tetris-x-colors and tetris-tty-colors
    ))

(defvar tetris-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar tetris-ghost-options
  '(((glyph colorize)
     (t ?\-))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar tetris-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tetris-shapes
  [
   ;   -1 0 1 2
   ;-1 [][][][]
   ; 0 [][][][]
   ; 1 [][][][]
   ; 2 [][][][]

   ; For placement purposes these need to have the top-left of their
   ; bounding boxes aligned to 0, 0

   ;0 O
   [[[0  0] [1  0] [0  1] [1  1]]]

   ;1 L
   [[[0  0] [1  0] [2  0] [2  1]] ;
    [[1 -1] [1  0] [1  1] [0  1]] ;
    [[0 -1] [0  0] [1  0] [2  0]] ;
    [[1 -1] [2 -1] [1  0] [1  1]]];

   ;2 J
   [[[0  0] [1  0] [2  0] [0  1]] ;
    [[0 -1] [1 -1] [1  0] [1  1]] ; 3
    [[2 -1] [0  0] [1  0] [2  0]] ; 012
    [[1 -1] [1  0] [1  1] [2  1]]];

   ;3 S
   [[[0  0] [1  0] [1  1] [2  1]] ;  23
    [[1  0] [0  1] [1  1] [0  2]]]; 01

   ;4 Z
   [[[1  0] [2  0] [0  1] [1  1]] ; 23
    [[0  0] [0  1] [1  1] [1  2]]];  01

   ;5 T
   [[[1  0] [0  1] [1  1] [2  1]] ;
    [[1  0] [1  1] [2  1] [1  2]] ; 123
    [[0  1] [1  1] [2  1] [1  2]] ;  0
    [[1  0] [0  1] [1  1] [1  2]]];

   ;6 I
   [[[0  0] [1  0] [2  0] [3  0]] ;0123
    [[1 -1] [1  0] [1  1] [1  2]]];
   ]
  "Each shape is described by a vector that contains the coordinates of
each one of its four blocks.")

;;the scoring rules were taken from "xtetris".  Blocks score differently
;;depending on their rotation

(defconst tetris-shape-scores
  [[6] [6 7 6 7] [6 7 6 7] [6 7] [6 7] [5 5 6 5] [5 8]] )

(defconst tetris-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst tetris-blank 7)

(defconst tetris-border 8)

(defconst tetris-space 9)

(defconst tetris-ghost 40)

(defun tetris-default-update-speed-function (_shapes rows)
  (/ 20.0 (+ 50.0 rows)))


;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-shape 0)
(defvar tetris-rot 0)
(defvar tetris-next-shape 0)
(defvar tetris-n-shapes 0)
(defvar tetris-n-rows 0)
(defvar tetris-score 0)
(defvar tetris-pos-x 0)
(defvar tetris-pos-y 0)
(defvar tetris-paused nil)

(defvar tetris-lockdown 0)
(defvar tetris-did-hold-shape nil)
(defvar tetris-hold-shape nil)
(defvar tetris-cur-bag '())

(make-variable-buffer-local 'tetris-shape)
(make-variable-buffer-local 'tetris-rot)
(make-variable-buffer-local 'tetris-next-shape)
(make-variable-buffer-local 'tetris-n-shapes)
(make-variable-buffer-local 'tetris-n-rows)
(make-variable-buffer-local 'tetris-score)
(make-variable-buffer-local 'tetris-pos-x)
(make-variable-buffer-local 'tetris-pos-y)
(make-variable-buffer-local 'tetris-paused)

(make-variable-buffer-local 'tetris-lockdown)
(make-variable-buffer-local 'tetris-did-hold-shape)
(make-variable-buffer-local 'tetris-hold-shape)
(make-variable-buffer-local 'tetris-cur-bag)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tetris-mode-map
  (let ((map (make-sparse-keymap 'tetris-mode-map)))
    (define-key map "n"		'tetris-start-game)
    (define-key map "q"		'tetris-end-game)
    (define-key map "p"		'tetris-pause-game)

    (define-key map "h"	    'tetris-move-left)
    (define-key map "j"	    'tetris-move-down)
    (define-key map "k"	    'tetris-rotate-next)
    (define-key map "l"	    'tetris-move-right)

    (define-key map "i"	    'tetris-swap-hold-shape)

    (define-key map " "		'tetris-move-bottom)
    (define-key map "d"	    'tetris-rotate-prev)
    (define-key map "f"	    'tetris-rotate-next)
    map))

(defvar tetris-null-map
  (let ((map (make-sparse-keymap 'tetris-null-map)))
    (define-key map "n"		'tetris-start-game)
    map))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetris-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c tetris-blank)
                   tetris-blank-options)
                  ((and (>= c 0) (<= c 6))
		   (append
		    tetris-cell-options
		    `((((glyph color-x) ,(aref tetris-x-colors c))
		       (color-tty ,(aref tetris-tty-colors c))
		       (t nil)))))
                  ((= c tetris-border)
                   tetris-border-options)
                  ((= c tetris-ghost)
                   tetris-ghost-options)
                  ((= c tetris-space)
                   tetris-space-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun tetris-get-tick-period ()
  (if (boundp 'tetris-update-speed-function)
      (let ((period (apply tetris-update-speed-function
			   tetris-n-shapes
			   tetris-n-rows nil)))
	(and (numberp period) period))))

(defun tetris-get-shape-cell (block)
  (aref (aref  (aref tetris-shapes
                     tetris-shape) tetris-rot)
        block))

(defun tetris-shape-width ()
  (aref (aref tetris-shape-dimensions tetris-shape) 0))

(defun tetris-shape-rotations ()
  (length (aref tetris-shapes tetris-shape)))

(defun tetris-draw-score ()
  (let ((strings (vector
         (format "Shapes: %05d" tetris-n-shapes)
         (format "Rows:   %05d" tetris-n-rows)
         (format "Score:  %05d" tetris-score)
)))
    (dotimes (y (length strings))
      (let* ((string (aref strings y))
             (len (length string)))
        (dotimes (x len)
          (gamegrid-set-cell (+ tetris-score-x x)
                             (+ tetris-score-y y)
                             (aref string x)))))))

(defun tetris-update-score ()
  (tetris-draw-score)
  (let ((period (tetris-get-tick-period)))
    (if period (gamegrid-set-timer period))))

(defun tetris-swap-hold-shape ()
  (interactive)
  (if tetris-did-hold-shape (ignore)
    ; else
    (let ((tetris-prev-shape tetris-shape))
      (tetris-pre-move) ; lift shape from board
      (if (not tetris-hold-shape)
        (tetris-new-shape)
        (tetris-new-shape tetris-hold-shape))
      (setq tetris-hold-shape tetris-prev-shape)
      (setq tetris-did-hold-shape t)))
  (tetris-draw-hold-shape)
)

(defun tetris-get-bag-shape ()
  (defun nshuffle (sequence)
    ; Knuth shuffle
    (cl-loop for i from (length sequence) downto 2
          do (cl-rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
    sequence)

  (if (not (car tetris-cur-bag))
      (setq tetris-cur-bag (nshuffle '(0 1 2 3 4 5 6))))
  (pop tetris-cur-bag)
)

(cl-defun tetris-new-shape (&optional (use-held-shape nil))
  ; use-held-shape should be nil to use the new shape, or the # of a specific shape to use
  (if use-held-shape
    (setq tetris-shape use-held-shape)
    ; Else
    (setq tetris-shape tetris-next-shape)
    (setq tetris-next-shape (tetris-get-bag-shape))
  )
  (setq tetris-did-hold-shape nil) ; always safe because swap-hold-shape wraps this function
  (setq tetris-rot 0)
  (setq tetris-pos-x (/ (- tetris-width (tetris-shape-width)) 2))
  (setq tetris-pos-y 0)
  (if (tetris-test-shape)
      (tetris-end-game)
    (tetris-post-move)
    (tetris-draw-next-shape)
    (tetris-draw-hold-shape)
    (tetris-update-score)))

(defun tetris-draw-next-shape ()
  (dotimes (x 4)
    (dotimes (y 4)
      (gamegrid-set-cell (+ tetris-next-x x)
                         (+ tetris-next-y y)
                         tetris-blank)))
  (dotimes (i 4)
    (let ((tetris-shape tetris-next-shape)
          (tetris-rot 0))
      (gamegrid-set-cell (+ tetris-next-x
                            (aref (tetris-get-shape-cell i) 0))
                         (+ tetris-next-y
                            (aref (tetris-get-shape-cell i) 1))
                         tetris-shape)))
)

(defun tetris-draw-hold-shape ()
  (dotimes (x 4)
    (dotimes (y 4)
      (gamegrid-set-cell (+ tetris-hold-x x)
                         (+ tetris-hold-y y)
                         tetris-blank)))
  (if tetris-hold-shape (dotimes (i 4)
    (let ((tetris-shape tetris-hold-shape)
          (tetris-rot 0))
      (gamegrid-set-cell (+ tetris-hold-x
                            (aref (tetris-get-shape-cell i) 0))
                         (+ tetris-hold-y
                            (aref (tetris-get-shape-cell i) 1))
                         tetris-shape))))
)

(defun tetris-pre-move ()
  (tetris-draw-shape tetris-blank tetris-pos-y)
  (tetris-draw-shape tetris-blank (tetris-get-ghost-y)))

(defun tetris-post-move ()
  (tetris-draw-shape tetris-ghost (tetris-get-ghost-y))
  (tetris-draw-shape tetris-shape tetris-pos-y))

(cl-defun tetris-draw-shape (&optional (celltype tetris-shape) &optional (py tetris-pos-y))
  "Draw the current shape to the game board, optionally with a specific brush and y position."
  (dotimes (i 4)
    (let ((c (tetris-get-shape-cell i)))
      (gamegrid-set-cell (+ tetris-top-left-x
                            tetris-pos-x
                            (aref c 0))
                         (+ tetris-top-left-y
                            py
                            (aref c 1))
                         celltype)))
)

(cl-defun tetris-test-shape
    ; Note reversed order here b/c x position is less frequently overwritten
    (&optional (py tetris-pos-y)
     &optional (px tetris-pos-x))
  (let ((hit nil))
    (dotimes (i 4)
      (unless hit
        (setq hit
              (let* ((c (tetris-get-shape-cell i))
                     (xx (+ px
                            (aref c 0)))
                     (yy (+ py
                            (aref c 1)))
                    (cell (gamegrid-get-cell
                         (+ xx tetris-top-left-x)
                         (+ yy tetris-top-left-y)))
                     )
                (or (>= xx tetris-width)
                    (>= yy tetris-height)
                    (and (>= cell 0) (<= cell 6)) ;is a shape
                    (= cell tetris-border)
                    )
                ))))
    hit))

(defun tetris-is-full-row (y)
  (let ((full t))
    (dotimes (x tetris-width)
      (if (= (gamegrid-get-cell (+ tetris-top-left-x x)
                                (+ tetris-top-left-y y))
             tetris-blank)
          (setq full nil)))
    full))

(defun tetris-shift-row (y)
  (if (= y 0)
      (dotimes (x tetris-width)
	(gamegrid-set-cell (+ tetris-top-left-x x)
			   (+ tetris-top-left-y y)
			   tetris-blank))
    (dotimes (x tetris-width)
      (let ((c (gamegrid-get-cell (+ tetris-top-left-x x)
                                  (+ tetris-top-left-y y -1))))
        (gamegrid-set-cell (+ tetris-top-left-x x)
                           (+ tetris-top-left-y y)
			   c)))))

(defun tetris-shift-down ()
  "Find complete rows and shift them down, incrementing score."
  (dotimes (y0 tetris-height) ; For all rows
    (when (tetris-is-full-row y0)
      (setq tetris-n-rows (1+ tetris-n-rows))
      (cl-loop for y from y0 downto 0 do
               (tetris-shift-row y)))))

(defun tetris-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      tetris-draw-border-with-glyphs))

(defun tetris-init-buffer ()
  (gamegrid-init-buffer tetris-buffer-width
			tetris-buffer-height
			tetris-space)
  (let ((buffer-read-only nil))
    (if (tetris-draw-border-p)
	(cl-loop for y from -1 to tetris-height do
                 (cl-loop for x from -1 to tetris-width do
                          (gamegrid-set-cell (+ tetris-top-left-x x)
                                             (+ tetris-top-left-y y)
                                             tetris-border))))
    (dotimes (y tetris-height)
      (dotimes (x tetris-width)
        (gamegrid-set-cell (+ tetris-top-left-x x)
                           (+ tetris-top-left-y y)
                           tetris-blank)))
    (if (tetris-draw-border-p)
	(cl-loop for y from -1 to 4 do
                 (cl-loop for x from -1 to 4 do
                          (gamegrid-set-cell (+ tetris-next-x x)
                                             (+ tetris-next-y y)
                                             tetris-border)
                          (gamegrid-set-cell (+ tetris-hold-x x)
                                             (+ tetris-hold-y y)
                                             tetris-border)
)
))))

(defun tetris-reset-game ()
  (gamegrid-kill-timer)
  (tetris-init-buffer)
  (setq tetris-next-shape (random 7))
  (setq tetris-shape	0
	tetris-rot	0
	tetris-pos-x	0
	tetris-pos-y	0
	tetris-n-shapes	0
	tetris-n-rows	0
	tetris-score	0
	tetris-paused	nil)
  (tetris-new-shape))

(defun tetris-shape-done ()
  "Executed when tetris shape locks in place. Updates, then adds new shape."
  (tetris-draw-shape tetris-shape)
  (tetris-shift-down)
  (setq tetris-n-shapes (1+ tetris-n-shapes))
  (setq tetris-score
	(+ tetris-score
	   (aref (aref tetris-shape-scores tetris-shape) tetris-rot)))
  (tetris-update-score)
  (setq tetris-lockdown 0)
  (tetris-new-shape))

(defun tetris-update-game (tetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not tetris-paused)
	   (eq (current-buffer) tetris-buffer))
   (let (hit)
     (tetris-pre-move)
     (setq tetris-pos-y (1+ tetris-pos-y))
     (setq hit (tetris-test-shape))
     (if hit ; If moving down collides, move back up.
	    (setq tetris-pos-y (1- tetris-pos-y)))
     (tetris-post-move)
     (if hit ; If moving down collided
	    (if (< tetris-lockdown tetris-lockdown-end)
          (setq tetris-lockdown (1+ tetris-lockdown))
          (tetris-shape-done))
      ) ; end if hit
    ) ; end let hit
  ) ; end if not paused
)

(defun tetris-get-ghost-y ()
  (let ((hit nil) (gy tetris-pos-y))
    (while (not hit)
      (setq gy (1+ gy))
      (setq hit (tetris-test-shape gy)))
    (setq gy (1- gy))
    gy)
)

(defun tetris-move-bottom ()
  "Drop the shape to the bottom of the playing area."
  (interactive)
  (unless tetris-paused
    (tetris-pre-move)
    (setq tetris-pos-y (tetris-get-ghost-y))
    (tetris-post-move)
    (tetris-shape-done)))

(defun tetris-move-left ()
  "Move the shape one square to the left."
  (interactive)
  (unless tetris-paused
    (tetris-pre-move)
    (setq tetris-pos-x (1- tetris-pos-x))
    (if (tetris-test-shape)
        (setq tetris-pos-x (1+ tetris-pos-x)))
    (tetris-post-move)
 ))

(defun tetris-move-right ()
  "Move the shape one square to the right."
  (interactive)
  (unless tetris-paused
    (tetris-pre-move)
    (setq tetris-pos-x (1+ tetris-pos-x))
    (if (tetris-test-shape)
	(setq tetris-pos-x (1- tetris-pos-x)))
    (tetris-post-move)
))

(defun tetris-move-down ()
  "Move the shape one square down."
  (interactive)
  (unless tetris-paused
    (tetris-tick-and-continue)))

(defun tetris-rotate (rot-offset)
  (setq tetris-rot (% (+ tetris-rot rot-offset)
                      (tetris-shape-rotations)))
  (let ((py tetris-pos-y) (px tetris-pos-x) (done nil))
    (dolist (ol tetris-wall-kick-table)
      (unless done
        (seq-let [ox oy] ol
          (let ((ny (+ tetris-pos-y oy)) (nx (+ tetris-pos-x ox)))
            (if (tetris-test-shape ny nx)
              (ignore) ; (message (format "BAD %s, %s, %s, %s" ox oy nx ny))
              ; Did not hit
              (setq tetris-pos-y ny)
              (setq tetris-pos-x nx)
              ; (message (format "OK %s, %s, %s, %s" ox oy nx ny))
              (setq done t)
            )
          )
        )
      )
    )
    (if (not done)
      (setq tetris-rot (% (+ tetris-rot 3)
                        (tetris-shape-rotations))))
  )
)

(defun tetris-rotate-prev ()
  "Rotate the shape clockwise."
  (interactive)
  (unless tetris-paused
      (tetris-pre-move)
      (tetris-rotate 3)
      (tetris-post-move)))

(defun tetris-rotate-next ()
  "Rotate the shape anticlockwise."
  (interactive)
  (unless tetris-paused
        (tetris-pre-move)
        (tetris-rotate 1)
        (tetris-post-move)))

(defun tetris-end-game ()
  "Terminate the current game."
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map tetris-null-map)
  (gamegrid-add-score tetris-score-file tetris-score))

(defun tetris-start-gamegrid-timer ()
  (let ((period (or (tetris-get-tick-period)
		    tetris-default-tick-period)))
    (gamegrid-start-timer period 'tetris-update-game)))

(defun tetris-start-game ()
  "Start a new game of Tetris."
  (interactive)
  (tetris-reset-game)
  (use-local-map tetris-mode-map)
  (tetris-start-gamegrid-timer))

(defun tetris-tick-and-continue ()
  "Force a game update to happen now, and reset the timer."
  (gamegrid-kill-timer)
  (tetris-update-game (current-buffer))
  (tetris-start-gamegrid-timer))

(defun tetris-pause-game ()
  "Pause (or resume) the current game."
  (interactive)
  (setq tetris-paused (not tetris-paused))
  (message (and tetris-paused "Game paused (press p to resume)")))

(defun tetris-active-p ()
  (eq (current-local-map) tetris-mode-map))

(put 'tetris-mode 'mode-class 'special)

(define-derived-mode tetris-mode nil "Tetris"
  "A mode for playing Tetris."

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map tetris-null-map)

  (unless (featurep 'emacs)
    (setq mode-popup-menu
	  '("Tetris Commands"
	    ["Start new game"	tetris-start-game]
	    ["End game"		tetris-end-game
	     (tetris-active-p)]
	    ["Pause"		tetris-pause-game
	     (and (tetris-active-p) (not tetris-paused))]
	    ["Resume"		tetris-pause-game
	     (and (tetris-active-p) tetris-paused)])))

  (setq show-trailing-whitespace nil)

  (setq gamegrid-use-glyphs tetris-use-glyphs)
  (setq gamegrid-use-color tetris-use-color)

  (gamegrid-init (tetris-display-options)))

;;;###autoload
(defun tetris ()
  "Play the Tetris game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-pause-game]	Pauses (or resumes) the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-move-down]	Moves the shape one square down
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)

  (select-window (or (get-buffer-window tetris-buffer-name)
		     (selected-window)))
  (switch-to-buffer tetris-buffer-name)
  (gamegrid-kill-timer)
  (tetris-mode)
  (tetris-start-game))

(defun quick-toggle ()
  (interactive)
  (winner-mode)
  (setq tetris-paused t)
  (ansi-term "top"))

(define-key tetris-mode-map (kbd "RET") 'quick-toggle)

(provide 'tetris)

;;; tetris.el ends here
