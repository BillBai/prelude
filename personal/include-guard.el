(defun get-include-guard ()
   "Return a string suitable for use in a C/C++ include guard"
   (let* ((fname (buffer-file-name (current-buffer)))
          (fbasename (file-relative-name fname (projectile-project-root)))
          (inc-guard-base (replace-regexp-in-string"[.-/]"
                                                    "_"
                                                    fbasename)))
     (concat 
      (upcase (projectile-project-name))
      "_"
      (upcase inc-guard-base)
      "_")))

(defun make-include-guard ()
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (when (string=".h"(substring file-name -2))
      (let ((include-guard (get-include-guard)))
        (insert "#ifndef " include-guard)
        (newline)
        (insert "#define " include-guard)
        (newline 4)
        (insert "#endif // ifndef " include-guard)
        (newline)
        (previous-line 3)
        (set-buffer-modified-p nil)))))

(provide 'include-guard)
;; include-guard.el ends here
