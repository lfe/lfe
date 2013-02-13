;;; inferior-lfe.el --- Inferior Lisp Flavoured Erlang mode
;;; Author Robert Virding
;;;
;;; Copied from inf-lisp and modified for LFE.

;;; Code:

(require 'comint)
(require 'lfe-mode)

(defvar inferior-lfe-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\C-x\C-e" 'lfe-eval-last-sexp)
    map)
  "Keymap for inferior LFE mode.")

;; (defvar inferior-lfe-mode-map nil)
;; (unless inferior-lfe-mode-map
;;   (setq inferior-lfe-mode-map (copy-keymap comint-mode-map))
;;   (set-keymap-parent inferior-lfe-mode-map lisp-mode-shared-map)
;;   (define-key inferior-lfe-mode-map "\C-x\C-e" 'lfe-eval-last-sexp))

(define-key lfe-mode-map "\C-x\C-e" 'lfe-eval-last-sexp) ;Gnu convention
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

(defun inferior-lfe-mode ()
  "Major mode for interacting with an inferior LFE process.

\\{inferior-lfe-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-lfe-mode-hook' (in that order).
"
  (interactive)
  (delay-mode-hooks (comint-mode))
  (setq major-mode 'inferior-lfe-mode)
  (setq mode-name "Inferior LFE")
  (setq mode-line-process '(":%s"))
  (lfe-mode-variables)
  (use-local-map inferior-lfe-mode-map)
  (setq comint-prompt-regexp inferior-lfe-prompt)
  (setq comint-input-filter (function lfe-input-filter))
  (setq comint-get-old-input (function lfe-get-old-input))
  (setq comint-process-echoes t)
  (run-mode-hooks 'inferior-lfe-mode-hook))

(defun lfe-input-filter (str)
  (not (string-match inferior-lfe-filter-regexp str)))

(defun lfe-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inferior-lfe (cmd)
  "Run an inferior LFE process, input and output via a buffer `*inferior-lfe*'."
;;   (interactive (list (if current-prefix-arg
;; 			 (read-string "Run LFE: " inferior-lfe-program)
;; 		       inferior-lfe-program)))
;;   (if (not (comint-check-proc "*inferior-lfe*"))
;;       (let ((cmdlist (split-string cmd)))
;; 	(set-buffer (apply (function make-comint)
;; 			   "inferior-lfe" (car cmdlist) nil (cdr cmdlist)))
;; 	(inferior-lfe-mode)))
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
    (when (not (comint-check-proc "*inferior-lfe*"))
      (set-buffer (apply (function make-comint)
			 "inferior-lfe" prog nil opts))
      (inferior-lfe-mode))
    (setq inferior-lfe-buffer "*inferior-lfe*")
    (pop-to-buffer "*inferior-lfe*")))

;; (apply (function make-comint)
;;        "inferior-lfe" "sh" nil
;;        (quote ("-i" "-c" ". /Users/rv/.bashrc ; lfe -env TERM vt100")))

(defalias 'run-lfe 'inferior-lfe)

(defun lfe-eval-region (start end &optional and-go)
  "Send the current region to the inferior LFE process.
Prefix argument means switch to the LFE buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inferior-lfe-proc) start end)
  (comint-send-string (inferior-lfe-proc) "\n")
  (if and-go (switch-to-lfe t)))

(defun lfe-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior LFE process.
Prefix argument means switch to the LFE buffer afterwards."
  (interactive "P")
  (lfe-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun switch-to-lfe (eob-p)
  "Switch to the inferior Lisp process buffer.
With argument, positions cursor at end of buffer."
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

(defun inferior-lfe-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-lfe-mode)
				      (current-buffer)
				    inferior-lfe-buffer))))
    (or proc
	(error "No LFE subprocess; see variable `inferior-lfe-buffer'"))))

;; The end.
(provide 'inferior-lfe)

(defvar inferior-lfe-load-hook nil
  "*Functions to run when Erlang mode is loaded.")

(run-hooks 'inferior-lfe-load-hook)
