;;; inferior-lfe.el --- Inferior Lisp Flavoured Erlang mode

;; Copyright (c) 2012-2020 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Author Robert Virding

;;; Commentary:
;; Copied from inf-lisp and modified for LFE.

;;; Code:

(require 'comint)
(require 'lfe-mode)

(defvar inferior-lfe-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\C-x\C-e" 'lfe-eval-last-sexp)
    (define-key map "\C-c\M-o" 'inferior-lfe-clear-buffer)
    (define-key map "\C-j" 'inferior-lfe-newline-and-maybe-indent)
    map)
  "Keymap for inferior LFE mode.")

;; (defvar inferior-lfe-mode-map nil)
;; (unless inferior-lfe-mode-map
;;   (setq inferior-lfe-mode-map (copy-keymap comint-mode-map))
;;   (set-keymap-parent inferior-lfe-mode-map lisp-mode-shared-map)
;;   (define-key inferior-lfe-mode-map "\C-x\C-e" 'lfe-eval-last-sexp))

(define-key lfe-mode-map "\C-x\C-e" 'lfe-eval-last-sexp) ; GNU convention
(define-key lfe-mode-map "\C-c\C-r" 'lfe-eval-region)
(define-key lfe-mode-map "\C-c\C-z" 'switch-to-lfe)
(define-key lfe-mode-map "\C-c\C-k" 'lfe-compile) ; Erlang mode convention

;; (defvar inferior-lfe-program "lfe -pa /Users/rv/erlang/lfe/ebin -env TERM vt100"
(defcustom inferior-lfe-program "lfe"
  "*Program name for invoking an inferior LFE in Inferior LFE mode."
  :group 'lfe
  :type 'string)

(defcustom inferior-lfe-program-options '("-pa" "/Users/rv/erlang/lfe/ebin")
  "*The options used when activating the LFE shell.

This must be a list of strings.
You may add the following command line options:
- \"-nobanner\"."
  :group 'lfe
  :type '(repeat string))

(defvar inferior-lfe-prompt "^[^>]*>+ *"
  "*Regexp to recognise prompts in the Inferior LFE mode.")

(defvar inferior-lfe-buffer nil
  "*The current inferior-lfe process buffer.")

(defvar inferior-lfe-mode-hook nil
  "*Hook for customizing Inferior LFE mode.")

(defvar inferior-lfe-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defvar inferior-lfe-indent-on-Cj nil
  "*Defines if on C-j the line is indented.")

(defvar inferior-lfe-check-if-rebar-project nil
  "*Checks if there is a `rebar.config' file within the 4 levels of nested folders.
If yes, then there will be a prompt on starting inferior-lfe
to choose whether to run the lfe process using rebar3,
else lfe will be run as usual.")

;;;###autoload
(defun inferior-lfe-mode ()
  "Major mode for interacting with an inferior LFE process.

\\{inferior-lfe-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-lfe-mode-hook' (in that order)."
  (interactive)
  (delay-mode-hooks (comint-mode))
  (setq major-mode 'inferior-lfe-mode)
  (setq mode-name "Inferior LFE")
  (setq mode-line-process '(":%s"))
  (lfe-mode-variables)
  (use-local-map inferior-lfe-mode-map)
  (setq comint-prompt-regexp inferior-lfe-prompt)
  (setq comint-prompt-read-only t)
  (setq comint-input-filter (function lfe-input-filter))
  (setq comint-get-old-input (function lfe-get-old-input))
  (setq comint-process-echoes t)
  (run-mode-hooks 'inferior-lfe-mode-hook))

(defun lfe-input-filter (str)
  "Predicate for filtering additions to input history.
Return nil if `STR` matches `inferior-lfe-filter-regexp', otherwise t."
  (not (string-match inferior-lfe-filter-regexp str)))

(defun lfe-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (max (point) (comint-line-beginning-position)) end))))

(defun inferior-lfe--is-rebar-project ()
  "Return the project root directory."
  (locate-dominating-file default-directory "rebar.config"))

(defun inferior-lfe--start-rebar-lfe ()
  (string= (read-answer "Rebar3 project detected. Start lfe repl using rebar3? "
                        '(("yes" ?y "use rebar3")
                          ("no" ?n "use regular lfe")))
           "yes"))

;;;###autoload
(defun inferior-lfe ()
  "Run an inferior LFE process, input and output via a buffer `*inferior-lfe*'.
Start the shell `inferior-lfe-program' `inferior-lfe-program-options' -env TERM vt100.
If a rebar project is found you are prompted (see `inferior-lfe-check-if-rebar-project')
and can choose to run lfe using rebar3."
  (interactive)
  (let ((prog inferior-lfe-program)
        (opts (append inferior-lfe-program-options
                      '("-env" "TERM" "vt100")))
        (rebar-project-root (inferior-lfe--is-rebar-project)))
    (when (and inferior-lfe-check-if-rebar-project
               rebar-project-root
               (inferior-lfe--start-rebar-lfe))
      (setq prog "sh"
            opts (list "-i" "-c" (concat "TERM=\"vt100\";"
                                         (format "cd %s" rebar-project-root)
                                         "; rebar3 lfe repl"))))
    (unless (comint-check-proc "*inferior-lfe*")
      (set-buffer (apply (function make-comint)
                         "inferior-lfe" prog nil opts))
      (inferior-lfe-mode))
    (setq inferior-lfe-buffer "*inferior-lfe*")
    (pop-to-buffer "*inferior-lfe*")))

;;;###autoload
(defalias 'run-lfe 'inferior-lfe)

(defun inferior-lfe-newline-and-maybe-indent ()
  "Sends a newline and indents the line when `inferior-lfe-indent-on-Cj' is true."
  (interactive)
  (save-restriction
    (narrow-to-region comint-last-input-start (point-max))
    (insert "\n")
    (when inferior-lfe-indent-on-Cj
      (lisp-indent-line))))

(defun lfe-eval-region (start end &optional and-go)
  "Send the current region (from `START' to `END') to the inferior LFE process.
`AND-GO' means switch to the LFE buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-lfe-proc) start end)
  (comint-send-string (inferior-lfe-proc) "\n")
  (if and-go (switch-to-lfe t)))

(defun lfe-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior LFE process.
`AND-GO' means switch to the LFE buffer afterwards."
  (interactive "P")
  (lfe-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun lfe-compile ()
  "Compiles the LFE module in the current buffer using the inferior LFE process."
  (interactive)
  (let ((file (buffer-file-name)))
    (comint-send-string (inferior-lfe-proc) "(c \"")
    (comint-send-string (inferior-lfe-proc) file)
    (comint-send-string (inferior-lfe-proc) "\")\n")))

(defun switch-to-lfe (eob-p)
  "Switch to the inferior Lisp process buffer.
When `EOB-P' is given, position cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-lfe-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inferior-lfe-buffer t))))
        (pop-to-buffer inferior-lfe-buffer))
    (run-lfe inferior-lfe-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inferior-lfe-clear-buffer ()
  "Delete the output generated by the LFE process."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun inferior-lfe-proc ()
  "Get the LFE subprocess."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-lfe-mode)
                                      (current-buffer)
                                    inferior-lfe-buffer))))
    (or proc
        (error "No LFE subprocess; see variable `inferior-lfe-buffer'"))))

;; The end.
(provide 'inferior-lfe)

(defvar inferior-lfe-load-hook nil
  "*Functions to run when Inferior LFE mode is loaded.")

(run-hooks 'inferior-lfe-load-hook)
;;; inferior-lfe.el ends here
