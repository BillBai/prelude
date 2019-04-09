;;; personal.el

;; re-bind projectile prefix key
(setq projectile-keymap-prefix (kbd "C-c p"))

;; set column limit to 1--
(setq whitespace-line-column 100)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; disable scroll bar for GUI
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; enable global linum mode
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


;; use ibuffer instead of helm-buffer-list
;; restore from the helm everywhere key binding
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; quick access to recent files
(recentf-mode 1) ; keep a list of recently opened files
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

;; M-x alternative key
(global-set-key (kbd "<f5>") 'execute-extended-command)

;; default frame size
(add-to-list 'default-frame-alist '(height . 67))
(add-to-list 'default-frame-alist '(width . 200))

;; some theme
(prelude-require-package 'solarized-theme)
(prelude-require-package 'spacemacs-theme)
(prelude-require-package 'material-theme)
(prelude-require-package 'gruvbox-theme)

;; prefer gruvbox for now~
(setq prelude-theme 'gruvbox)
(load-theme 'gruvbox t)


;; make sure we got use-package
(prelude-require-package 'use-package)


;;;
;;Text Encoding
;;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors  - https://github.com/magnars/multiple-cursors.el
;; Allows you to have multiple cursors on different lines so you can
;; easily edit multiple lines at once.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'multiple-cursors)
(use-package multiple-cursors
  :ensure t
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c C-a" . mc/mark-all-like-this)
         ("C-c C-e" . mc/edit-lines))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vlf - handle open very large files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'vlf)
(use-package vlf
  :ensure t)


;;;
;; Org
;;;

(eval-after-load "org"
  '(require 'ox-md nil t))

(progn
  ;; org-mode setup

  ;; when opening a org file, don't collapse headings
  (setq org-startup-folded nil)

  ;; wrap long lines. don't let it disappear to the right
  (setq org-startup-truncated nil)

  ;; when in a url link, enter key should open it
  (setq org-return-follows-link t)

  ;; make org-mode” syntax color embedded source code
  (setq org-src-fontify-natively t))

(setq org-agenda-files (file-expand-wildcards "~/Dropbox/GTD/*.org"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'cmake-mode)
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))
                          ))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'protobuf-mode)
(use-package protobuf-mode
  :mode ("\\.proto"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode (".json" ".imp"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all))
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
:mode (".md" ".markdown"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure GN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'gn-mode)
(add-to-list 'auto-mode-alist '("\\.gn\\'"    . gn-mode))


;;;
;; Project Management
;;;

(prelude-require-package 'neotree)

(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)

(setq-default neo-show-hidden-files t)



;;;
;; Python
;;;

(prelude-require-package 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(setq venv-location "~/.virtualenvs/")



;;;
;; Misc
;;;

;; nyan nyan nyan~~~
(prelude-require-package 'nyan-mode)
(nyan-mode)

(prelude-require-package 'whitespace-cleanup-mode)


;;;
;; LaTeX
;;;

(prelude-require-package 'auctex)
(add-hook 'LaTeX-mode-hook (lambda()
                             (add-to-list
                              'TeX-command-list
                              '("XeLaTeX" "%`xelatex%(mode)%' %t"
                                TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil)
                             (setq TeX-show-compilation t)))


;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

(prelude-require-package 'reason-mode)

(add-to-list 'auto-mode-alist '("\\.re\\'" . reason-mode))

(prelude-require-package 'utop)

(setq utop-command "opam config exec -- rtop -emacs")
(add-hook 'reason-mode-hook #'utop-minor-mode) ;; can be included in the hook above as well

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; personal.el ends here
