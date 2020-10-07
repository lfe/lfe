;;; lfe-start.el --- Initialise the LFE mode package

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

;;; Author: Robert Virding

;;; Commentary:

;;; Code:

;; Declare autoload functions in lfe-mode.el and inferior-lfe.el.
(autoload 'lfe-mode "lfe-mode" "Major mode for editing LFE code." t)
(autoload 'lfe-indent-function "lfe-indent" "Indent LFE." t)
(autoload 'inferior-lfe-mode "inferior-lfe"
  "Major mode for interacting with an inferior LFE process." t)
(autoload 'inferior-lfe "inferior-lfe" "Run an LFE process." t)
(autoload 'run-lfe "inferior-lfe" "Run an LFE process." t)

;; Associate ".lfe" with LFE mode.
(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode) t)

;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
(dolist (lfe-ext '(".beam" ".jam" ".vee"))
  (add-to-list 'completion-ignored-extensions lfe-ext))

(provide 'lfe-start)
;;; lfe-start.el ends here
