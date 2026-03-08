(defhydra hydra-dash
  (:color blue)
  "Quickstart"
  ("s p"
   (find-file-other-frame "/ssh:pi@pearl:~")
   "ssh-pearl" :column "ssh")

  ("d h" (find-file "~") "~" :column "dir")
  ("d d" (find-file "~/dotfiles/") "~/dotfiles/")

  ("c b" (bash) "bash" :column "cmd" :color pink)
  ("q" nil :column "meta"))
(global-set-key (kbd "C-c .") #'hydra-dash/body)
