;;; personal.el

;;; custom packages
(prelude-require-package 'use-package)

;; Some themes
(prelude-require-package 'solarized-theme)
(prelude-require-package 'spacemacs-theme)
(prelude-require-package 'material-theme)
(prelude-require-package 'gruvbox-theme)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)


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

;; M-x alternative key
(global-set-key (kbd "<f5>") 'execute-extended-command)

;; default frame size
(add-to-list 'default-frame-alist '(height . 67))
(add-to-list 'default-frame-alist '(width . 200))

;; prefer gruvbox for now~
(setq prelude-theme 'gruvbox)
(load-theme 'gruvbox t)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                          ))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure GN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in .emacs.d/vendor
(require 'gn-mode)
(add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(global-set-key [f9] 'neotree-project-dir)

(setq-default neo-show-hidden-files t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
;; (setq venv-location "~/.wetlands/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'auctex)
(add-hook 'LaTeX-mode-hook (lambda()
                             (add-to-list
                              'TeX-command-list
                              '("XeLaTeX" "%`xelatex%(mode)%' %t"
                                TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil)
                             (setq TeX-show-compilation t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reason setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-package 'reason-mode)
(add-to-list 'auto-mode-alist '("\\.re\\'" . reason-mode))

(prelude-require-package 'utop)
(setq utop-command "opam config exec -- rtop -emacs")
(add-hook 'reason-mode-hook #'utop-minor-mode) ;; can be included in the hook above as well

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nyan nyan nyan~~~
(prelude-require-package 'nyan-mode)
(nyan-mode)

;; other useful packages
(prelude-require-packages '(projectile-ripgrep
                            editorconfig
                            auto-package-update
                            origami
                            beacon
                            rainbow-delimiters
                            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save ~/.emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (byte-compile-file (expand-file-name file)))
  )

(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (file-truename "~/.emacs.d/personal/personal.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (file-truename "~/.emacs.d/personal/personal.el"))))))

;; Byte-compile again to ~/.emacs.elc if it is outdated
(if (file-newer-than-file-p
     (file-truename "~/.emacs.d/init.el")
     (file-truename "~/.emacs.d/init.elc"))
    (byte-compile-init-files "~/.emacs.d/init.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "I will update packages now"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck-ycmd
(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(use-package flycheck-pyflakes
  :ensure t
  :after python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :ensure t
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase)
         )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell Mode for Spelling Corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function flyspell-goto-next-error "flyspell.el")
    (declare-function flyspell-mode "flyspell.el")
    (declare-function flyspell-prog-mode "flyspell.el"))
  (setq flyspell-issue-welcome-flag nil)
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

  (global-set-key (kbd "<f7>") 'flyspell-buffer)
  (global-set-key (kbd "<f8>") 'flyspell-correct-previous)

  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  )
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)


;; personal.el ends here
