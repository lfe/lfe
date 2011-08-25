;;; lfe-start.el - Initialise the LFE mode package.
;;; Author: Robert Virding

;; Declare autoload functions in lfe-mode.el and inferior-lfe.el.
(autoload 'lfe-mode "lfe-mode" "Major mode for editing LFE code." t)
(autoload 'inferior-lfe-mode "inferior-lfe" "Major mode for interacting with an inferior LFE process." t)
(autoload 'inferior-lfe "inferior-lfe" "Run an LFE process." t)
(autoload 'run-lfe "inferior-lfe" "Run an LFE process." t)

;; Associate ".lfe" with LFE mode.
(let ((a '("\\.lfe\\'" . lfe-mode)))
  (or (assoc (car a) auto-mode-alist)
      (setq auto-mode-alist (cons a auto-mode-alist))))

;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
(let ((lfe-ext '(".beam" ".jam" ".vee")))
  (while lfe-ext
    (unless (member (car lfe-ext) completion-ignored-extensions)
      (setq completion-ignored-extensions
	    (cons (car lfe-ext) completion-ignored-extensions)))
    (setq lfe-ext (cdr lfe-ext))))

(provide 'lfe-start)
