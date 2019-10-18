;;;
;; C / C++ configs
;;;

(prelude-require-packages '(clang-format
                            google-c-style                            
                            lsp-mode
                            ccls
                            dap-mode
                            lsp-treemacs
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp setup
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :hook ((c++-mode . lsp-deferred)
         (c-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  ;; Prefer using lsp-ui (flycheck) over flymake.
  (setq lsp-enable-file-watchers nil)
  (setq lsp-prefer-flymake nil) )

(use-package lsp-ui
  :requires lsp-mode flycheck
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :commands company-lsp
  :ensure t
  :config (push 'company-lsp company-backends))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

(use-package dap-lldb)

(use-package ccls
  :hook
  ((c-mode c++-mode objc-mode) .
   (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable (file-truename "~/Developer/ccls/Release/ccls"))
  (setq ccls-sem-highlight-method 'font-lock)
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  
  ;; https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el#L10
  (defun ccls/callee ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind)
    (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind)
    (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

  ;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h
  ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
  (defun ccls/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 128)))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 64)))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :excludeRole 32)))

  ;; References w/ Role::Read
  (defun ccls/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8)))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))
  )

(provide 'cpp)
;; cpp.el ends here
