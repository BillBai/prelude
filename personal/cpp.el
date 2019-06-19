;;;
;; C / C++ configs
;;;

(prelude-require-packages '(clang-format
                            google-c-style
                            cmake-mode                               
                            flycheck
                            lsp-mode
                            dap-mode
                            ccls
                            lsp-ui
                            company-box
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

(use-package company-box
  :hook (company-mode . company-box-mode))


;; (use-package dap-mode
;;   :config
;;   (dap-mode 1)
;;   (require 'dap-gdb-lldb)
;;   (use-package dap-ui
;;     :ensure nil
;;     :config
;;     (dap-ui-mode 1))
;;   )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package modern-cpp-font-lock
;;     :ensure t
;;     :init
;;     (eval-when-compile
;;         ;; Silence missing function warnings
;;         (declare-function modern-c++-font-lock-global-mode
;;             "modern-cpp-font-lock.el"))
;;     :config
;;     (modern-c++-font-lock-global-mode t)
;;     )

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
;; (use-package counsel-etags
;;     :ensure t
;;     :init
;;     (eval-when-compile
;;         ;; Silence missing function warnings
;;         (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
;;         (declare-function counsel-etags-guess-program "counsel-etags.el")
;;         (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
;;     :bind (
;;               ("M-." . counsel-etags-find-tag-at-point)
;;               ("M-t" . counsel-etags-grep-symbol-at-point)
;;               ("M-s" . counsel-etags-find-tag))
;;     :config
;;     ;; Ignore files above 800kb
;;     (setq counsel-etags-max-file-size 800)
;;     ;; Ignore build directories for tagging
;;     (add-to-list 'counsel-etags-ignore-directories '"build*")
;;     (add-to-list 'counsel-etags-ignore-directories '"out*")
;;     (add-to-list 'counsel-etags-ignore-directories '".vscode")
;;     (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
;;     (add-to-list 'counsel-etags-ignore-filenames "TAGS")
;;     (add-to-list 'counsel-etags-ignore-filenames "*.json")
;;     ;; Don't ask before rereading the TAGS files if they have changed
;;     (setq tags-revert-without-query t)
;;     ;; Don't warn when TAGS files are large
;;     (setq large-file-warning-threshold nil)
;;     ;; How many seconds to wait before rerunning tags for auto-update
;;     ;; (setq counsel-etags-update-interval 180)
;;     (setq counsel-etags-stop-auto-update-tags t)
;;     ;; Set up auto-update
;;     (add-hook
;;         'prog-mode-hook
;;         (lambda () (add-hook 'after-save-hook
;;                        (lambda ()
;;                            (counsel-etags-virtual-update-tags))))
;;         )
;;     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; ;; cloned ycmd directory
;; (defvar my:ycmd-server-command (list "python3" (file-truename "~/Developer/ycmd/ycmd")))
;; (defvar my:ycmd-extra-conf-whitelist (list (file-truename "~/.ycm_extra_conf.py")))
;; (defvar my:ycmd-global-config (file-truename "~/.ycm_extra_conf.py"))

;; ;; Compilation command for C/C++
;; (defvar my:compile-command "clang++ -Wall -Wextra -std=c++14")

;; ;; Set up YouCompleteMe for emacs:
;; ;; https://github.com/Valloric/ycmd
;; ;; https://github.com/abingham/emacs-ycmd
;; (defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
;; (if (not my:python-location)
;;     (message
;;         "Could not start YouCompleteMeDaemon because the python executable could
;; not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
;; appropriately in ~/.emacs.el.\n" (nth 0 my:ycmd-server-command)))
;; (if (not (file-directory-p (nth 1 my:ycmd-server-command)))
;;     (message "Could not YouCompleteMeDaemon because the specified directory does
;; not exist.\nSpecified directory is: '%s'
;; Please set my:ycmd-server-command appropriately in ~/.emacs.el.\n"
;;         (nth 1 my:ycmd-server-command)))
;; (if (and my:python-location
;;         (file-directory-p (nth 1 my:ycmd-server-command)))
;;     (use-package ycmd
;;         :ensure t
;;         :init
;;         (eval-when-compile
;;             ;; Silence missing function warnings
;;             (declare-function global-ycmd-mode "ycmd.el"))
;;         (add-hook 'after-init-hook #'global-ycmd-mode)
;;         :config
;;         (progn
;;             (set-variable 'ycmd-server-command my:ycmd-server-command)
;;             (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
;;             (set-variable 'ycmd-global-config my:ycmd-global-config)
;;             (setq ycmd-force-semantic-completion t)
;;             (use-package company-ycmd
;;                 :ensure t
;;                 :init
;;                 (eval-when-compile
;;                     ;; Silence missing function warnings
;;                     (declare-function company-ycmd-setup "company-ycmd.el"))
;;                 :config
;;                 (company-ycmd-setup)
;;                 )

;;             (use-package flycheck-ycmd
;;                 :ensure t
;;                 :init
;;                 (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
;;                 )

;;             ;; Add displaying the function arguments in mini buffer using El Doc
;;             (require 'ycmd-eldoc)
;;             (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
;;             )
;;         )
;;     )

(provide 'cpp)
;; cpp.el ends here
