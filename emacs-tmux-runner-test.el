(require 'buttercup)
(require 'emacs-tmux-runner)

(load "test-helper")

(describe "with-default-target macro"
  (it "sets session to 1, window to 2 and pane to 1"
    (with-default-target
     (expect *etr:session* :to-equal "1")
     (expect *etr:window*  :to-equal "2")
     (expect *etr:pane*    :to-equal "3"))))

(describe "etr:vsplit"
  (before-each (stub-shell))

  (it "sends split-window command"
    (etr:vslipt)
    (expect-shell-cmd "tmux split-window")))

(describe "etr:hsplit"
  (before-each (stub-shell))

  (it "sends the command to split window horizontally"
    (etr:hsplit)
    (expect-shell-cmd "tmux split-window -h")))

(describe "etr:target-pane"
  (it "formats to the expected tmux format"
    (let ((*etr:session* "1") (*etr:window* "2") (*etr:pane* "3"))
      (expect (etr:target-pane) :to-equal "1:2.3"))

    (let ((*etr:session* "3") (*etr:window* "2") (*etr:pane* "1"))
      (expect (etr:target-pane) :to-equal "3:2.1"))))

(describe "etr:send-keys"
  (before-each (stub-shell))

  (it "sends to the target pane"
    (with-default-target
     (etr:send-keys "ls")
     (expect-cmd-to-default-target)))

  (it "disables key name lookup (processes as UTF8)"
    (with-default-target
     (etr:send-keys "ls")
     (expect (sent-cmd) :to-contain-substr "-l ls")))

  (it "ends with the provided arg"
    (with-default-target
     (etr:send-keys "whoami")
     (expect (sent-cmd) :to-end-with "whoami")))

  (it "doesn't press enter / doesn't send C-m"
    (with-default-target
     (etr:send-keys "ls")
     (expect (sent-cmd) :not :to-end-with-enter)))

  (it "allows commands to be sent partially"
    (with-default-target
     (etr:send-keys "who")
     (expect (sent-cmd) :to-end-with "who")

     (etr:send-keys "am")
     (expect (sent-cmd) :to-end-with "am")

     (etr:send-keys "i")
     (expect (sent-cmd) :to-end-with "i"))))

(describe "etr:send-command"
  (before-each (stub-shell))

  (it "sends to the target pane"
    (with-default-target
     (etr:send-command "ls")
     (expect-cmd-to-default-target)))

  (it "disabled key name lookup (processes as UTF8)"
    (with-default-target
     (etr:send-command "ls")
     (expect (first-sent-cmd) :to-contain-substr "-l ls")))

  (it "sends two commands, one for the actual command and another for <CR>"
    (with-default-target
     (etr:send-command "ls")
     (expect (nth-sent-cmd 0) :to-end-with "ls")
     (expect (nth-sent-cmd 1) :to-end-with-enter))))
