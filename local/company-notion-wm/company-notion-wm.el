
(defun company-notion-wm--candidates ()
  "Candidates handler for the company backend."
  (cons :async
        (lambda (cb)
          (company-notion-wm-inject-lua-helper)
          (let* ((context (lua-funcname-at-point))
                 (raw-result (notion-wm-cmd
                              (format "emacs.completion_candidates(\"%s\")" context)))
                 (result (split-string (read raw-result))))
            (funcall cb result)))))

(defun company-notion-wm--prefix ()
  (unless (company-in-string-or-comment)
    (or (company-grab-symbol-cons "\\." 1)
        'stop)))

(defun company-notion-wm-inject-lua-helper ()
  (notion-wm-send-string 
   "
      if emacs == undefined then
        emacs = {}
        function emacs.completion_candidates(str)
          completions = mod_query.do_complete_lua(_ENV, str)
          el_list = ''
          for i, c in ipairs(completions) do
            el_list = el_list..c..' '
          end
          return el_list
        end
      end"
   ))

(defu company-notion-wm (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-notion-wm))
    (prefix (company-notion-wm--prefix))
    (candidates (company-notion-wm--candidates))))

(provide 'company-notion-wm)
