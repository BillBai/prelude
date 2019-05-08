;;;
;; C / C++ configs
;;;

(prelude-require-package 'use-package)
(prelude-require-packages '(counsel-etags
                               clang-format
                               modern-cpp-font-lock
                               cc-mode
                               cmake-mode
                               counsel
                               flycheck
                               google-c-style
                               ycmd
                               company-ycmd
                               flycheck-ycmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration/Customization:
;; Defines global variables that are later used to customize and set
;; up packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; cloned ycmd directory
(defvar my:ycmd-server-command (list "python3" (file-truename "~/Developer/ycmd/ycmd")))
(defvar my:ycmd-extra-conf-whitelist (list (file-truename "~/.ycm_extra_conf.py")))
(defvar my:ycmd-global-config (file-truename "~/.ycm_extra_conf.py"))

;; Compilation command for C/C++
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++14")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
    :ensure t
    :bind (("C-c C-f r" . clang-format-region)
              ("C-c C-f b" . clang-format-buffer))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modern-cpp-font-lock
    :ensure t
    :init
    (eval-when-compile
        ;; Silence missing function warnings
        (declare-function modern-c++-font-lock-global-mode
            "modern-cpp-font-lock.el"))
    :config
    (modern-c++-font-lock-global-mode t)
    )

;; enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
    :ensure t
    :init
    (eval-when-compile
        ;; Silence missing function warnings
        (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
        (declare-function counsel-etags-guess-program "counsel-etags.el")
        (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
    :bind (
              ("M-." . counsel-etags-find-tag-at-point)
              ("M-t" . counsel-etags-grep-symbol-at-point)
              ("M-s" . counsel-etags-find-tag))
    :config
    ;; Ignore files above 800kb
    (setq counsel-etags-max-file-size 800)
    ;; Ignore build directories for tagging
    (add-to-list 'counsel-etags-ignore-directories '"build*")
    (add-to-list 'counsel-etags-ignore-directories '"out*")
    (add-to-list 'counsel-etags-ignore-directories '".vscode")
    (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    ;; How many seconds to wait before rerunning tags for auto-update
    (setq counsel-etags-update-interval 180)
    ;; Set up auto-update
    (add-hook
        'prog-mode-hook
        (lambda () (add-hook 'after-save-hook
                       (lambda ()
                           (counsel-etags-virtual-update-tags))))
        )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up YouCompleteMe for emacs:
;; https://github.com/Valloric/ycmd
;; https://github.com/abingham/emacs-ycmd
(defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
(if (not my:python-location)
    (message
        "Could not start YouCompleteMeDaemon because the python executable could
not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
appropriately in ~/.emacs.el.\n" (nth 0 my:ycmd-server-command)))
(if (not (file-directory-p (nth 1 my:ycmd-server-command)))
    (message "Could not YouCompleteMeDaemon because the specified directory does
not exist.\nSpecified directory is: '%s'
Please set my:ycmd-server-command appropriately in ~/.emacs.el.\n"
        (nth 1 my:ycmd-server-command)))
(if (and my:python-location
        (file-directory-p (nth 1 my:ycmd-server-command)))
    (use-package ycmd
        :ensure t
        :init
        (eval-when-compile
            ;; Silence missing function warnings
            (declare-function global-ycmd-mode "ycmd.el"))
        (add-hook 'after-init-hook #'global-ycmd-mode)
        :config
        (progn
            (set-variable 'ycmd-server-command my:ycmd-server-command)
            (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
            (set-variable 'ycmd-global-config my:ycmd-global-config)
            (setq ycmd-force-semantic-completion t)
            (use-package company-ycmd
                :ensure t
                :init
                (eval-when-compile
                    ;; Silence missing function warnings
                    (declare-function company-ycmd-setup "company-ycmd.el"))
                :config
                (company-ycmd-setup)
                )

            (use-package flycheck-ycmd
                :ensure t
                :init
                (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
                )

            ;; Add displaying the function arguments in mini buffer using El Doc
            (require 'ycmd-eldoc)
            (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
            )
        )
    )

(provide 'cpp)
;; cpp.el ends here
