(defvar inferior-term-shell "/bin/bash")
(defvar inferior-term-shell-switches nil)

(defvar inferior-term-buffer nil)
(make-variable-buffer-local 'inferior-term-buffer)

(defvar ith-command "\"%s\"")
(make-variable-buffer-local 'ith-command)

(defun ith-set-command ()
  (interactive)
  (setq-local ith-command (completing-read "Format string to execute? " '(
    "%s"
    "j2 %s"
    "python3 %s"
    "python3 -m pytest %s"
    ))))

(defun ith-run-command ()
  (interactive)
  ;; (term-send-string inferior-term-buffer (kbd "C-c C-l"))
  (accept-process-output (get-buffer-process inferior-term-buffer) 0 10)
  (term-send-string inferior-term-buffer
    (concat (format ith-command (concat "\"" (file-relative-name buffer-file-name default-directory) "\"")) "\n")))

(defun add-inferior-term-hook (hookfn)
  (setq inferior-term-buffer
    (apply 'make-term
      (generate-new-buffer-name (concat "ith-" (buffer-name)))
      "/bin/bash"
      nil
      inferior-term-shell-switches))
  ;; (display-buffer-in-side-window inferior-term-buffer '((side . right)))
  (display-buffer-in-side-window inferior-term-buffer '())
  (add-hook 'after-save-hook hookfn nil t))

(defun remove-inferior-term-hook (hookfn)
  (kill-buffer inferior-term-buffer)
  (remove-hook 'after-save-hook hookfn t))

(defun ith-start ()
  (interactive)
  (add-inferior-term-hook 'ith-run-command)
  (ith-run-command))

(defun ith-stop ()
  (interactive)
  (remove-inferior-term-hook 'ith-run-command))

(provide 'inferior-term)
