;;# Skeleton recipes

(defvar make-skeleton-saved-winconf nil)
(defvar make-skeleton-header ";; help for skeleton
;; (find-w3m \"http://www.panix.com/~tehom/my-code/skel-recipe.txt\")
;; (describe-function 'skeleton-insert)
;; These lines are ignored.
"
  "Help string for skeleton.")

(defun make-skeleton ()
  "Create skeleton of skeleton.
It is based on `A recipe for using skeleton.el'.
http://www.panix.com/~tehom/my-code/skel-recipe.txt

C-c C-e: Erase the skeleton contents.
C-c C-c: Finish the input.
"
  (interactive)
  (setq make-skeleton-saved-winconf (current-window-configuration))
  (switch-to-buffer "*make skeleton*")
  (make-skeleton-mode)
  (if (zerop (buffer-size))
      (make-skeleton-erase-buffer)))

(defun make-skeleton-finish ()
  (interactive)
  (set-window-configuration make-skeleton-saved-winconf)
  (insert "(define-skeleton ")
  (save-excursion
    (insert "-skeleton-\n"
            "\"Insert \" nil\n")
    (let ((lines (with-current-buffer "*make skeleton*"
                   ;; skip header
                   (goto-char (point-min))
                   (re-search-forward "^[^;]")
                   (beginning-of-line)
                   (split-string (buffer-substring (point) (point-max)) "\n"))))
      (dolist (line lines nil)
        (back-to-indentation)
        (insert (format "%S > \\n\n" line))))
    (insert ")\n")))

(defun make-skeleton-erase-buffer ()
  "Erase the skeleton contents."
  (interactive)
  (erase-buffer)
  (insert make-skeleton-header))


(define-derived-mode make-skeleton-mode fundamental-mode "skeleton"
  "Major mode for creating a skeleton of skeleton."
  (define-key make-skeleton-mode-map "\C-c\C-c" 'make-skeleton-finish)
  (define-key make-skeleton-mode-map "\C-c\C-e" 'make-skeleton-erase-buffer)
  )

;;# Skeletons

(dolist (modehook '(python-mode-hook))
  (add-hook modehook
    (defalias (intern (concat "skeldef-" (symbol-name modehook)))
      (lambda ()
        (define-skeleton skel-copyright-py
          "Insert n52" nil
          "# Copyright "
          (shell-command-to-string "echo -n $(date +%Y)")
          )
        ))))

(dolist (modehook '(js-mode-hook))
  (add-hook modehook
    (defalias (intern (concat "skeldef-" (symbol-name modehook)))
      (lambda ()
        (define-skeleton skel-copyright-js
          "Insert n52" nil
          "/*" > \n
          " * Copyright "
          (shell-command-to-string "echo -n $(date +%Y)")
          " */" > \n
          )
        ))))


