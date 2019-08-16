;;;
;; C / C++ configs
;;;

(prelude-require-packages '(clang-format
                            google-c-style
                            lsp-mode
                            ccls
                            lsp-ui
                            company-lsp))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
    :ensure t
    :bind (("C-c C-f r" . clang-format-region)
              ("C-c C-f b" . clang-format-buffer)))

(add-hook 'c-mode-common-hook
          (function (lambda () (local-set-key (kbd "TAB") 'clang-format-region))))

;; google c style, treat .h file as c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ccls setup and lsp
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(setq ccls-executable (file-truename "~/Developer/ccls/Release/ccls"))
(use-package ccls
  :hook
  ((c-mode c++-mode c-mode) .
   (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  ;; For rainbow semantic highlighting
  ;; (ccls-use-default-rainbow-sem-highlight)
  )


(provide 'cpp)
;; cpp.el ends here
