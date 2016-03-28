(defun docs (module file)
  (let* ((`#(ok [#(ok ,_ ,beam)]) (lfe_comp:file file '[binary]))
         (`#(ok ,_ ,chunks) (beam_lib:all_chunks beam)))
    (binary_to_term (proplists:get_value "LDoc" chunks))))

(lfe_io:format "~p\n" `[,(docs 'example "./test/example.lfe")])

;; $ cd /path/to/lfe
;; $ make
;; $ ./bin/lfe test/docs.lfe
