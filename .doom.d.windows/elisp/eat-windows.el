;;; elisp/eat-windows.el -*- lexical-binding: t; -*-

;; eat windows support from https://codeberg.org/akib/emacs-eat/pulls/126

(defun eat--build-command(command switches width height)
  "Build command to be executed with args.

COMMAND is going to be run with SWITCHES.  WIDTH and HEIGHT are
terminal dimensions."
  (cond
   ((eq system-type 'windows-nt)
    `("conhost.exe" "--headless" "--height" ,(number-to-string height)
      "--width" ,(number-to-string width) "--feature" "pty" ,command
      ,@switches))
   (t
    `("/usr/bin/env" "sh" "-c"
      ,(format "stty -nl echo rows %d columns \
  %d sane 2>%s ; if [ $1 = .. ]; then shift; fi; exec \"$@\""
               height
               width
               null-device)
      ".."
      ,command
      ,@switches))))

(defun eat-exec (buffer name command startfile switches)
  "Start up a process in BUFFER for Eat mode.

  Run COMMAND with SWITCHES.  Set NAME as the name of the process.
  Blast any old process running in the buffer.  Don't set the buffer
  mode.  You can use this to cheaply run a series of processes in the
  same Eat buffer.  The hook `eat-exec-hook' is run after each exec."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when-let* ((eat-terminal)
                  (proc (eat-term-parameter
                         eat-terminal 'eat--process)))
        (remove-hook 'eat-exit-hook #'eat--kill-buffer t)
        (delete-process proc))
      ;; Ensure final newline.
      (goto-char (point-max))
      (unless (or (= (point-min) (point-max))
                  (= (char-before (point-max)) ?\n))
        (insert ?\n))
      (unless (= (point-min) (point-max))
        (insert "\n\n"))
      (setq eat-terminal (eat-term-make buffer (point)))
      (eat-semi-char-mode)
      (when-let* ((window (get-buffer-window nil t)))
        (with-selected-window window
          (eat-term-resize eat-terminal (window-max-chars-per-line)
                           (floor (window-screen-lines)))))
      (setf (eat-term-parameter eat-terminal 'input-function)
            #'eat--send-input)
      (setf (eat-term-parameter eat-terminal 'set-cursor-function)
            #'eat--set-cursor)
      (setf (eat-term-parameter eat-terminal 'grab-mouse-function)
            #'eat--grab-mouse)
      (setf (eat-term-parameter
             eat-terminal 'manipulate-selection-function)
            #'eat--manipulate-kill-ring)
      (setf (eat-term-parameter eat-terminal 'ring-bell-function)
            #'eat--bell)
      (setf (eat-term-parameter eat-terminal 'set-cwd-function)
            #'eat--set-cwd)
      (setf (eat-term-parameter eat-terminal 'ui-command-function)
            #'eat--handle-uic)
      (eat--set-term-sixel-params)
      ;; Crank up a new process.
      (let* ((size (eat-term-size eat-terminal))
             (process-environment
              (nconc
               (list
                (concat "TERM=" (eat-term-name))
                (concat "TERMINFO=" eat-term-terminfo-directory)
                (concat "INSIDE_EMACS=" eat-term-inside-emacs)
                (concat "EAT_SHELL_INTEGRATION_DIR="
                        eat-term-shell-integration-directory))
               process-environment))
             (process-connection-type t)
             ;; We should suppress conversion of end-of-line format.
             (inhibit-eol-conversion t)
             (process
              (make-process
               :name name
               :buffer buffer
               :command (eat--build-command command switches
                                            (car size) (cdr size))
               :filter #'eat--filter
               :sentinel #'eat--sentinel
               :file-handler t)))
        (process-put process 'adjust-window-size-function
                     #'eat--adjust-process-window-size)
        (set-process-query-on-exit-flag
         process eat-query-before-killing-running-terminal)
        ;; Jump to the end, and set the process mark.
        (goto-char (point-max))
        (set-marker (process-mark process) (point))
        (setf (eat-term-parameter eat-terminal 'eat--process)
              process)
        (setf (eat-term-parameter eat-terminal 'eat--input-process)
              process)
        (setf (eat-term-parameter eat-terminal 'eat--output-process)
              process)
        (when eat-kill-buffer-on-exit
          (add-hook 'eat-exit-hook #'eat--kill-buffer 90 t))
        ;; Feed it the startfile.
        (when startfile
          ;; This is guaranteed to wait long enough
          ;; but has bad results if the shell does not prompt at all
          ;;          (while (= size (buffer-size))
          ;;            (sleep-for 1))
          ;; I hope 1 second is enough!
          (sleep-for 1)
          (goto-char (point-max))
          (insert-file-contents startfile)
          (process-send-string
           process (delete-and-extract-region (point) (point-max)))))
      (eat-term-redisplay eat-terminal))
    (run-hook-with-args 'eat-exec-hook (eat-term-parameter
                                        eat-terminal 'eat--process))
    buffer))

(defun eat-make (name program &optional startfile &rest switches)
  "Make a Eat process NAME in a buffer, running PROGRAM.

    The name of the buffer is made by surrounding NAME with `*'s.  If
    there is already a running process in that buffer, it is not
    restarted.  Optional third arg STARTFILE is the name of a file to send
    the contents of to the process.  SWITCHES are the arguments to
    PROGRAM."
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    ;; If no process, or nuked process, crank up a new one and put
    ;; buffer in Eat mode.  Otherwise, leave buffer and existing
    ;; process alone.
    (when (not (let ((proc (get-buffer-process buffer)))
                 (and proc (memq (process-status proc)
                                 '(run stop open listen connect)))))
      (with-current-buffer buffer
        (eat-mode))
      (eat-exec buffer name program startfile switches))
    buffer))

(defun eat--1 (program arg display-buffer-fn)
  "Start a new Eat terminal emulator in a buffer.

    PROGRAM and ARG is same as in `eat' and `eat-other-window'.
    DISPLAY-BUFFER-FN is the function to display the buffer."
  (let* ((program (or program (funcall eat-default-shell-function)))
         (args
          (cond
           ((eq system-type 'windows-nt)
            `("sh.exe" nil ("-c" ,(format "'%s'" program))))
           (t
            `("/usr/bin/env" nil (list "sh" "-c" ,program)))))
         (buffer
          (cond
           ((numberp arg)
            (get-buffer-create (format "%s<%d>" eat-buffer-name arg)))
           (arg
            (generate-new-buffer eat-buffer-name))
           (t
            (get-buffer-create eat-buffer-name)))))
    (with-current-buffer buffer
      (unless (eq major-mode #'eat-mode)
        (eat-mode))
      (funcall display-buffer-fn buffer)
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (apply #'eat-exec buffer (buffer-name) args))
      buffer)))

(provide 'eat-windows)
;;; eat-windows.el ends here
