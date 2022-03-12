(defvar mdl--undo-tree-call-now nil
  "Flag to indicate `mdl--coq-undo-tree-do' should call the real
`undo-tree-undo' or `undo-tree-redo' on behalf of `undo'.")

(defvar mdl--pg-undo-is-redoing nil
  "Non-nil if `pg-protected-undo' was called by
`mdl-pg-protected-redo'.")

(defun mdl-pg-protected-redo (&optional arg)
  "As `pg-protected-undo', but redo instead."
  (interactive "*P")
  (let ((mdl--pg-undo-is-redoing t))
    (pg-protected-undo arg)))

(defun mdl--coq-undo-tree-do (fn &rest args)
  (apply (if (not (eq major-mode 'coq-mode))
             fn
           (if mdl--undo-tree-call-now
               fn
             #'pg-protected-undo))
         args))

(defun mdl-advice-undo (fn &rest args)
  ;; `pg-protected-undo-1' assumes `undo' will set `last-command', but we've
  ;; advised `undo' with `undo-tree-undo' (which uses `primitive-undo'
  ;; directly), so we have to make sure to set `last-command' ourselves.
  (if (and (eq major-mode 'coq-mode)
           undo-tree-mode)
      (prog1
          (let ((real-fun (if mdl--pg-undo-is-redoing
                              #'undo-tree-redo
                            #'undo-tree-undo))
                (mdl--undo-tree-call-now t))
            (apply real-fun args))
        (setq last-command 'undo))
    (apply fn args)))

(defun mdl-hook-coq-mode ()
  (when (fboundp 'undo-tree-mode)
    (undo-tree-mode 1)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map undo-tree-map)
      (define-key map [remap undo-tree-undo] 'pg-protected-undo)
      (define-key map [remap undo-tree-redo] 'mdl-pg-protected-redo)
      (add-to-list 'minor-mode-overriding-map-alist
                   `(undo-tree-mode . ,map)))

    (advice-add 'undo :around #'mdl-advice-undo)
    (advice-add 'undo-tree-undo :around #'mdl--coq-undo-tree-do)
    (advice-add 'undo-tree-redo :around #'mdl--coq-undo-tree-do)))

(add-hook 'company-coq-mode-hook 'mdl-hook-coq-mode)