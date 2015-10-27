;;; inferior-lfe.el --- Inferior Lisp Flavoured Erlang mode

;; Copyright (c) 2012-2013 Robert Virding
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

;; (defvar inferior-lfe-program "lfe -pa /Users/rv/erlang/lfe/ebin -env TERM vt100"
(defvar inferior-lfe-program "lfe"
  "*Program name for invoking an inferior LFE in Inferior LFE mode.")

(defvar inferior-lfe-program-options '("-pa" "/Users/rv/erlang/lfe/ebin")
  "*The options used when activating the LFE shell.

This must be a list of strings.")

(defvar inferior-lfe-prompt "^[^>]*>+ *"
  "*Regexp to recognise prompts in the Inferior LFE mode.")

(defvar inferior-lfe-buffer nil
  "*The current inferior-lfe process buffer.")

(defvar inferior-lfe-mode-hook nil
  "*Hook for customizing Inferior LFE mode.")

(defvar inferior-lfe-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

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
      (buffer-substring (point) end))))

;;;###autoload
(defun inferior-lfe (cmd)
  "Run an inferior LFE process, input and output via a buffer `*inferior-lfe*'.
If `CMD' is given, use it to start the shell, otherwise:
`inferior-lfe-program' `inferior-lfe-program-options' -env TERM vt100."
  ;; (interactive (list (if current-prefix-arg
  ;;                        (read-string "Run LFE: " inferior-lfe-program)
  ;;                      inferior-lfe-program)))
  ;; (if (not (comint-check-proc "*inferior-lfe*"))
  ;;     (let ((cmdlist (split-string cmd)))
  ;;       (set-buffer (apply (function make-comint)
  ;;                          "inferior-lfe" (car cmdlist) nil (cdr cmdlist)))
  ;;       (inferior-lfe-mode)))
  (interactive (list (if current-prefix-arg
                         (read-string "Run LFE: ")
                       ())))
  (let (prog opts)
    (if cmd
        (setq prog "sh"
              opts (list "-i" "-c" cmd))
      (setq prog inferior-lfe-program
            opts (append inferior-lfe-program-options
                         '("-env" "TERM" "vt100"))))
    (unless (comint-check-proc "*inferior-lfe*")
      (set-buffer (apply (function make-comint)
                         "inferior-lfe" prog nil opts))
      (inferior-lfe-mode))
    (setq inferior-lfe-buffer "*inferior-lfe*")
    (pop-to-buffer "*inferior-lfe*")))

;; (apply (function make-comint)
;;        "inferior-lfe" "sh" nil
;;        (quote ("-i" "-c" ". /Users/rv/.bashrc ; lfe -env TERM vt100")))

;;;###autoload
(defalias 'run-lfe 'inferior-lfe)

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
