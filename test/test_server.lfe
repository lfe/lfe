;; Copyright (c) 2008-2013 Robert Virding
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

;; As close as we can get to a vanilla erlang if, a case with no match.
(defmacro eif args
   (fletrec ((r ([(list* t v as)] `((_ (when ,t) ,v) . ,(r as)))
                ([()] ())))
     `(case 1 . ,(r args))))

(defmacro test-pat (pat expr)
  `(let* ((val ,expr)
          (,pat val))
     val))

;; We don't have any sensible line numbers to save so we save form.
(defmacro line (expr)
  `(progn (put 'test_server_loc (tuple (MODULE) ',expr))
          ,expr))

(defmacro config args
  `(: test_server lookup_config . ,args))
