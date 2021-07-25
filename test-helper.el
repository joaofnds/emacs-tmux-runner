(setf sh-cmd 'shell-command-to-string)

(defun stub-shell ()
  (spy-on sh-cmd))

(defun expect-shell-cmd (cmd)
  (expect sh-cmd :to-have-been-called-with cmd))

(defun expect-cmd-to-target (session window pane)
  (expect (sent-cmd)
          :to-match
          (format "-t %s:%s.%s" session window pane)))

(defun expect-cmd-to-default-target ()
  (expect-cmd-to-target *etr:session* *etr:window* *etr:pane*))

(defun sent-cmd ()
  (caar (last (spy-calls-all-args sh-cmd))))

(defun cmds-sent-count ()
  (spy-calls-count sh-cmd))

(defmacro with-default-target (&rest body)
  `(let ((*etr:session* "1") (*etr:window* "2") (*etr:pane* "3"))
     ,@body))
