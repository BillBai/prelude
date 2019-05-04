;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reason ML setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'reason-mode)
(add-to-list 'auto-mode-alist '("\\.re\\'" . reason-mode))

(prelude-require-package 'utop)
(setq utop-command "opam config exec -- rtop -emacs")
(add-hook 'reason-mode-hook #'utop-minor-mode) ;; can be included in the hook above as well

(provide 'reasonml)
;; reasonml.el ends here
