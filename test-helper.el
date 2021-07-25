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

(defun nth-sent-cmd (n)
  (car (nth n (spy-calls-all-args sh-cmd))))

(defun first-sent-cmd ()
  (nth-sent-cmd 0))

(defun last-sent-cmd ()
  (caar (last (spy-calls-all-args sh-cmd))))

(defalias 'sent-cmd 'last-sent-cmd)

(defun cmds-sent-count ()
  (spy-calls-count sh-cmd))

(defmacro with-default-target (&rest body)
  `(let ((*etr:session* "1") (*etr:window* "2") (*etr:pane* "3"))
     ,@body))

(buttercup-define-matcher-for-binary-function
    :to-end-with (lambda (str suffix)
                   (string-match (concat suffix "$") str))
  :expect-match-phrase "Expected %a to end with %b"
  :expect-mismatch-phrase "Expected %a NOT to end with %b")

(buttercup-define-matcher-for-binary-function
    :to-contain-substr (lambda (str substr)
                         (cl-search substr str))
  :expect-match-phrase "Expected %a to contain %b"
  :expect-mismatch-phrase "Expected %a NOT to contain %b")

(buttercup-define-matcher-for-unary-function
    :to-end-with-enter (lambda (cmd) (string-match " C-m$" cmd))
  :expect-match-phrase "Expected %a to end with ' C-m'"
  :expect-mismatch-phrase "Expected %a NOT to end with ' C-m'")
